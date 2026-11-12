/- # Formal Semantics of Verilog
   Expression evaluation, statement execution, module-level transition function construction.
-/

import VerilLean.Lib.Lib
import VerilLean.Lang.Syntax
import VerilLean.Lang.Analysis

namespace VerilLean.Lang.Semantics

open VerilLean.Lang.Syntax
open VerilLean.Lang.Analysis (getIOIds)
open VerilLean.Lib

-- ## SF monad infrastructure

inductive TrsFail where
  | fatal
  | undeclared
  | undriven
  | notSupported
  | notUnfoldable
  deriving Inhabited, Repr, BEq

abbrev trsOk (A : Type) := Except TrsFail A

-- Lift an `Option` into `trsOk`, mapping `none` to the given failure.
def liftOption (o : Option A) (f : TrsFail) : trsOk A :=
  match o with
  | some a => .ok a
  | none => .error f

-- Fallback bind: if `sf` fails, use `default` instead; then continue with `cont`.
def sfWithDefault (sf : trsOk A) (default : A) (cont : A → trsOk B) : trsOk B :=
  match sf with
  | .ok a => cont a
  | .error _ => cont default

-- Fallback return: if `sf` fails, return `ret`; otherwise continue with `cont`.
def sfOrReturn (sf : trsOk A) (ret : trsOk B) (cont : A → trsOk B) : trsOk B :=
  match sf with
  | .ok a => cont a
  | .error _ => ret

-- ## List operations on trsOk

def sfListMap (f : A → trsOk S) : List A → trsOk (List S)
  | [] => .ok []
  | a :: rest => do let s ← f a; let ss ← sfListMap f rest; pure (s :: ss)

def sfListMap2 (f : A → B → trsOk S) : List A → List B → trsOk (List S)
  | [], _ => .ok []
  | _, [] => .ok []
  | a :: as_, b :: bs => do let s ← f a b; let ss ← sfListMap2 f as_ bs; pure (s :: ss)

def iterate (f : T → A → trsOk A) : List T → A → trsOk A
  | [], a => .ok a
  | t :: ts, a => do let a' ← f t a; iterate f ts a'

def iterateResume (f : T → A → trsOk A) : List T → A → trsOk A
  | [], a => .ok a
  | t :: ts, a => sfWithDefault (f t a) a (iterateResume f ts)

def iterateUpdate (fitr : T → A → trsOk B) (fba : B → A) (nilb : B)
    (upda : A → A → A) (updb : B → B → B) : List T → A → trsOk B
  | [], _ => .ok nilb
  | t :: ts, a => do
    let b ← fitr t a
    let a' := upda a (fba b)
    let brest ← iterateUpdate fitr fba nilb upda updb ts a'
    pure (updb b brest)

-- ## Core types

abbrev Value := HMap

/- A machine state is always a string-keyed root map.  Missing lookups and
   missing statement results are represented with `Option Value`, never with a
   distinguished state-shaped `HMap` value. -/
structure State where
  fields : List (VId × Value)
  deriving Inhabited, Repr, BEq, ReflBEq, LawfulBEq

namespace State

def empty : State := ⟨[]⟩

def toValue (state : State) : Value := .str state.fields

def ofValue? : Value → Option State
  | .str fields => some ⟨fields⟩
  | _ => none

private def findValue? : Value → HPath → Option Value
  | value, [] => some value
  | .bits bits, .ind index :: rest =>
      findValue? (.bits (bits.select index.norm)) rest
  | .arr values, .ind index :: rest => do
      let (_, value) ← values.find? (fun entry => entry.1 == index.norm)
      findValue? value rest
  | .str fields, .vid name :: rest => do
      let (_, value) ← fields.find? (fun entry => entry.1 == name)
      findValue? value rest
  | _, _ => none

def find? (state : State) (path : HPath) : Option Value :=
  findValue? state.toValue path

def get? (state : State) (name : VId) : Option Value :=
  state.find? [.vid name]

private def setBit (current index bit : SZ) : SZ :=
  let mask := BitVec.ofNat current.width (1 <<< index.norm.toNat)
  let updated :=
    if bit.isZero then current.val &&& ~~~mask
    else current.val ||| mask
  { current with val := updated }

private def setValue : Value → HPath → Value → Value
  | _, [], value => value
  | .bits bits, [.ind index], .bits bit =>
      .bits (setBit bits index bit)
  | .bits bits, .ind _ :: _, _ => .bits bits
  | .arr values, .ind index :: rest, value =>
      .arr (hiterArr (fun current => setValue current rest value)
        (setValue .empty rest value) index.norm values)
  | _, .ind index :: rest, value =>
      .arr [(index.norm, setValue .empty rest value)]
  | .str fields, .vid name :: rest, value =>
      .str (hiterStr (fun current => setValue current rest value)
        (setValue .empty rest value) name fields)
  | _, .vid name :: rest, value =>
      .str [(name, setValue .empty rest value)]

def set (state : State) (path : HPath) (value : Value) : State :=
  match setValue state.toValue path value with
  | .str fields => ⟨fields⟩
  | _ => state

def setUsing (state fallback : State) (path : HPath) (value : Value) : State :=
  match path with
  | .vid name :: _ =>
      match state.get? name, fallback.get? name with
      | none, some current => (state.set [.vid name] current).set path value
      | _, _ => state.set path value
  | _ => state.set path value

def singleton (path : HPath) (value : Value) : State :=
  State.empty.set path value

def merge (state updates : State) : State :=
  match VerilLean.Lib.hupds state.toValue updates.toValue with
  | .str fields => ⟨fields⟩
  | _ => state

def mergeWhere (predicate : VId → Bool) (state updates : State) : State :=
  match phupds predicate state.toValue updates.toValue with
  | .str fields => ⟨fields⟩
  | _ => state

def filter (names : List VId) (state : State) : State :=
  ⟨hfilterStr names state.fields⟩

-- Remove names when entering/leaving a lexical frame.
def without (names : List VId) (state : State) : State :=
  ⟨state.fields.filter (fun entry => !names.contains entry.1)⟩

def child? (state : State) (name : VId) : Option State := do
  let value ← state.get? name
  State.ofValue? value

def nested (name : VId) (state : State) : State :=
  ⟨[(name, state.toValue)]⟩

end State

abbrev Flops := State
abbrev Decls := State
abbrev NW := State      -- new wire values
abbrev IFW := State     -- inputs + flops + wires
abbrev IFF := IFW × Flops

structure MTrs where
  inputVids  : List VId
  inputDecls : Decls
  outputVids : List VId
  func       : State → State → (State × State)

structure Func where
  inputVids      : List VId
  func           : List VId → IFW → NW → State → trsOk Value
  returnShape    : Value -- width determination needs ouput shape
  inputShapes      : List Value -- for width determination as well
abbrev TrsFMap (A : Type) := VId → trsOk A

def fmapEmpty : TrsFMap A := fun _ => .error .undeclared

def fmapSingle (k : VId) (v : A) : TrsFMap A :=
  fun k' => if k == k' then .ok v else .error .undeclared

def fmapMerge (m1 m2 : TrsFMap A) : TrsFMap A :=
  fun k => match m1 k with
    | .ok v => .ok v
    | .error _ => m2 k

abbrev MTrss := TrsFMap MTrs
abbrev Funcs := TrsFMap Func

private def guardFunctionCalls (funcs : Funcs) (callStack : List VId) : Funcs :=
  fun fid =>
    if callStack.contains fid then .error .notSupported
    else do
      let f ← funcs fid
      pure { f with
        func := fun _ ifw nw inputState =>
          f.func (fid :: callStack) ifw nw inputState }

-- ## Helpers

-- Try to find a path in `h1` first, then `h2`.
def findState2 (p : HPath) (h1 h2 : State) : Option Value :=
  h1.find? p <|> h2.find? p

-- Build a state from parallel lists of variable ids and values.
def buildFInputState (vids : List VId) (args : List Value) : State :=
  ⟨vids.zip args⟩

def buildOptionalInputState (vids : List VId) (args : List (Option Value)) : State :=
  ⟨(vids.zip args).filterMap fun
    | (vid, some value) => some (vid, value)
    | (_, none) => none⟩

-- ## Literal evaluation

def evalPriLiteral : primary_literal → Value
  | .number (.integral (.decimal (.unsigned v))) =>
      HMap.bits (SZ.mk' v sz_int32 true)
  | .number (.integral (.decimal (.base_unsigned none v))) =>
      HMap.bits (SZ.mk' v sz_int32 false)
  | .number (.integral (.decimal (.base_unsigned (some sz) v))) =>
      HMap.bits (SZ.mk' v sz false)
  | .number (.integral (.binary none v)) =>
      HMap.bits (SZ.mk' (binaryZtoZ sz_int32 v) sz_int32 false)
  | .number (.integral (.binary (some sz) v)) =>
      HMap.bits (SZ.mk' (binaryZtoZ sz v) sz false)
  | .number (.integral (.octal none v)) =>
      HMap.bits (SZ.mk' (octalZtoZ sz_int32 v) sz_int32 false)
  | .number (.integral (.octal (some sz) v)) =>
      HMap.bits (SZ.mk' (octalZtoZ sz v) sz false)
  | .number (.integral (.hex none v)) =>
      HMap.bits (SZ.mk' v sz_int32 false)
  | .number (.integral (.hex (some sz) v)) =>
      HMap.bits (SZ.mk' v sz false)
  | .unbased_unsized .zeros => HMap.bits (SZ.mk' 0 1 false)
  | .unbased_unsized .ones => HMap.bits (SZ.mk' 1 1 false)

-- ## Operator function dispatch

def uniOpFunc : unary_operator → SZ → SZ
  | .plus  => id
  | .minus => SZ.uMinus
  | .not   => SZ.uNot
  | .neg   => SZ.uNeg
  | .and   => SZ.uAnd
  | .nand  => SZ.uNand
  | .or    => SZ.uOr
  | .nor   => SZ.uNor
  | .xor   => SZ.uXor
  | .xnor  => SZ.uXnor

def binOpFunc : binary_operator → SZ → SZ → SZ
  | .add   => SZ.bAdd
  | .sub   => SZ.bSub
  | .mul   => SZ.bMul
  | .div   => SZ.bDiv
  | .rem   => SZ.bRem
  | .eq    => SZ.bEq
  | .neq   => SZ.bNEq
  | .feq   => SZ.bFEq
  | .fneq  => SZ.bFNEq
  | .weq   => SZ.bWEq
  | .wneq  => SZ.bWNEq
  | .land  => SZ.bLAnd
  | .lor   => SZ.bLOr
  | .pow   => SZ.bPow
  | .lt    => SZ.bLt
  | .le    => SZ.bLe
  | .gt    => SZ.bGt
  | .ge    => SZ.bGe
  | .band  => SZ.bAnd
  | .bor   => SZ.bOr
  | .bxor  => SZ.bXor
  | .bxnor => SZ.bXnor
  | .shr   => SZ.bShr
  | .shl   => SZ.bShl
  | .sar   => SZ.bSar
  | .sal   => SZ.bSal

-- ## Assign operator to binary operator

def assignOpToBinOp : assign_op → Option binary_operator
  | .eq   => none
  | .add  => some .add
  | .sub  => some .sub
  | .mul  => some .mul
  | .div  => some .div
  | .rem  => some .rem
  | .band => some .band
  | .bor  => some .bor
  | .bxor => some .bxor
  | .shl  => some .shl
  | .shr  => some .shr
  | .sal  => some .sal
  | .sar  => some .sar

-- ## System functions

-- $clog2 — ceil(log2 n), the width to index n items (IEEE 1800-2023 §20.8.1)
def clog2Nat (n : Nat) : Int :=
  if n ≤ 1 then 0 else (Nat.log2 (n - 1) + 1 : Nat)

-- ## Constant-expression evaluation

abbrev Consts := HMap

-- Type-name → resolved default-value shape (for user typedefs / enum names).
abbrev TDefs := HMap

structure ModuleCtx where
  tdefs  : TDefs
  decls  : Decls
  funcs  : Funcs
  consts : Consts
  deferUndriven : Bool := true
  localNames : List VId := []
  hidden : State := State.empty
  widthElaboratedModule: Option module_decl := none -- the module after widths are elaborated

def cfind (consts : Consts) (vid : VId) : trsOk Value :=
  match haccessO consts vid with
  | some v => .ok v
  | none => .error .undeclared

-- Require a scalar bit-vector at an operation boundary.  Structured values
-- must never be reinterpreted as the zero-width default returned by `hbits`.
def expectBits (v : Value) : trsOk SZ :=
  liftOption (hbitsO v) .notSupported

def expectBitsList (vs : List Value) : trsOk (List SZ) :=
  sfListMap expectBits vs

mutual
def evalConst (consts : Consts) : constant_expression → trsOk Value
  | .primary_literal pl => pure (evalPriLiteral pl)
  | .ident vid => cfind consts vid
  | .hierarchical_ident pe ce => do
      let pv ← evalConst consts pe
      let cvid ← match ce with
        | .ident vid => .ok vid
        | _ => .error .notSupported
      pure (haccess pv cvid)
  | .select te se => do
      let tv ← evalConst consts te
      let sv ← evalConst consts se
      let ssz ← expectBits sv
      pure (hselect tv ssz)
  | .select_const_range se lr rr => do
      let sv ← evalConst consts se
      let lv ← evalConst consts lr
      let rv ← evalConst consts rr
      let lsz ← expectBits lv
      let rsz ← expectBits rv
      pure (hrange sv lsz rsz)
  | .select_indexed_range_add se lr rr => do
      let sv ← evalConst consts se
      let lv ← evalConst consts lr
      let rv ← evalConst consts rr
      let lsz ← expectBits lv
      let rsz ← expectBits rv
      let hi := SZ.bAdd lsz (SZ.bSub rsz (SZ.mk' 1 rsz.width false))
      pure (hrange sv hi lsz)
  | .select_indexed_range_sub se lr rr => do
      let sv ← evalConst consts se
      let lv ← evalConst consts lr
      let rv ← evalConst consts rr
      let lsz ← expectBits lv
      let rsz ← expectBits rv
      let lo := SZ.bSub lsz (SZ.bSub rsz (SZ.mk' 1 rsz.width false))
      pure (hrange sv lsz lo)
  | .concat es => do
      let vs ← evalConstList consts es
      let szs ← expectBitsList vs
      pure (HMap.bits (SZ.concat szs))
  | .mult_concat ne ces => do
      let nv ← evalConst consts ne
      let cvs ← evalConstList consts ces
      let nsz ← expectBits nv
      let cszs ← expectBitsList cvs
      let count := nsz.norm.toNat
      pure (HMap.bits (SZ.rep count (SZ.concat cszs)))
  | .tf_call _ _ => .error .notSupported
  | .system_tf_call .signed aes =>
      match aes with
      | [ae] => do
          let av ← evalConst consts ae
          let asz ← expectBits av
          pure (HMap.bits asz.toSigned)
      | _ => .error .notSupported
  | .system_tf_call .unsigned aes =>
      match aes with
      | [ae] => do
          let av ← evalConst consts ae
          let asz ← expectBits av
          pure (HMap.bits asz.toUnsigned)
      | _ => .error .notSupported
  | .system_tf_call .clog2 aes =>
      match aes with
      | [ae] => do
          let av ← evalConst consts ae
          pure (HMap.bits (SZ.mk' (clog2Nat (hbits av).norm.toNat) sz_int32 true))
      | _ => .error .notSupported
  | .cast sze e => do
      let szv ← evalConst consts sze
      let ev ← evalConst consts e
      let sz ← expectBits szv
      let esz ← expectBits ev
      pure (HMap.bits (SZ.castV sz esz))
  | .unary_op op e => do
      let ev ← evalConst consts e
      let esz ← expectBits ev
      pure (HMap.bits (uniOpFunc op esz))
  | .inc_or_dec _ => .error .notSupported
  | .binary_op op le re => do
      let lv ← evalConst consts le
      let rv ← evalConst consts re
      let lsz ← expectBits lv
      let rsz ← expectBits rv
      pure (HMap.bits (binOpFunc op lsz rsz))
  | .cond ce te fe => do
      let cv ← evalConst consts ce
      let tv ← evalConst consts te
      let fv ← evalConst consts fe
      let csz ← expectBits cv
      if csz.isZero then pure fv else pure tv
  | .inside ie res => do
      let iv ← evalConst consts ie
      let rvs ← evalConstList consts res
      let isz ← expectBits iv
      let rszs ← expectBitsList rvs
      let isMatch := rszs.any (SZ.equiv isz)
      if isMatch then pure (HMap.bits (SZ.mk' 1 1 false)) else pure (HMap.bits (SZ.mk' 0 1 false))

def evalConstList (consts : Consts) : List constant_expression → trsOk (List Value)
  | [] => .ok []
  | e :: es => do
      let v ← evalConst consts e
      let vs ← evalConstList consts es
      pure (v :: vs)
end

-- ## getAccessVid — extract vid from a "child" expression

def getAccessVid : expression → trsOk VId
  | .ident vid => .ok vid
  | _ => .error .notSupported

-- ## Width Determination
/- Expressions are evaluated at widths that are sometimes dependent on the entire expression (context dtermined width) not
simple the variabbles declared widths . E.g. [8:0] z = x(8bits) + y(8bits) will widen x and y and do 9 bit addition.
Here the Algorithm is defined, which traverses the expression tree twice and casts to the correct width based on the expression.
Determine gets widths bottom up and propagate goes top down and changes widhts based on the rules of the verilog standard
-/


-- a tree type to hold the widths of the expression
inductive WidthTree where
| node (children: List WidthTree) (width: Nat) (signed:Bool)
deriving Repr

def WidthTree.width (tree:WidthTree): Nat :=
  match tree with
  | node _ w _ => w

def WidthTree.signed (tree:WidthTree): Bool :=
  match tree with
  | node _ _ s => s

def valSigned (v: Value): Bool :=
 match v with
 | .bits sz => sz.signed
 | _ => false
-- helper finds declaration shape and handles multi index recursively
def declShape (ctx: ModuleCtx) (expr: expression): trsOk Value := do
  match expr with
  |.ident name =>
    liftOption (haccessO ctx.consts name <|> ctx.decls.get? name) .undeclared
  |.select te _idx =>
      let shape ← declShape ctx te
      match shape with
      | .bits _ => pure (HMap.bits (SZ.mk' 0 1 false))
      | .arr  ((_, elem) :: _) => pure elem
      | _ => .error .notSupported
  |.hierarchical_ident pe ce =>
      let pv ← declShape ctx pe
      let cvid ← getAccessVid ce
      liftOption (haccessO pv cvid ) .undeclared
  | _ => pure (HMap.bits (SZ.mk' 0 1 false))   -- for indexing non name expressions from cast, concat
def valWidth (v: Value) : Nat :=
  match v with
    | .bits sz => sz.width
    | _ => 0 -- array names have width 0

mutual
def determine (ctx : ModuleCtx) (expr:expression): trsOk WidthTree := do
  match expr with
  | .primary_literal pl =>
    let sz ← expectBits (evalPriLiteral pl)
    pure (WidthTree.node [] sz.width sz.signed)
  | .ident _ =>
    let val ← declShape ctx expr
    let w := valWidth val
    pure (WidthTree.node [] w (valSigned val))
  | .hierarchical_ident pe _ =>
     let val ← declShape ctx expr
     let w := valWidth val
     pure (WidthTree.node [(← determine ctx pe)] w (valSigned val))
  | .select te se =>
    let val ← declShape ctx expr
    let w := valWidth val
    pure (WidthTree.node  [(← determine ctx te) , (←  determine ctx se)]  w (valSigned val))
  | .select_const_range se lr rr =>
    let val ← declShape ctx se
    match val with
    | .bits _ =>
      let lr_v := ( ← expectBits (←  evalConst ctx.consts (exprToConst lr))).norm
      let rr_v := (← expectBits (←  evalConst ctx.consts (exprToConst rr))).norm
      pure (WidthTree.node [ (← determine ctx se ), (← determine ctx lr ), (← determine ctx rr ) ] (((max lr_v rr_v) - (min lr_v rr_v)  + 1)).toNat false)
    | .arr _ => pure (WidthTree.node [ (← determine ctx se ), (← determine ctx lr ), (← determine ctx rr ) ] 0 false)
    | _ => .error .notSupported
  | .select_indexed_range_add se lr rr | .select_indexed_range_sub se lr rr =>
    let val ← declShape ctx se
    match val with
    | .bits _ =>
      let width := ( ← expectBits (←  evalConst ctx.consts (exprToConst rr))).norm
      pure (WidthTree.node [ (← determine ctx se ), (← determine ctx lr ), (← determine ctx rr ) ] width.toNat false)
    | .arr _ => pure (WidthTree.node [ (← determine ctx se ), (← determine ctx lr ), (← determine ctx rr ) ] 0 false)
    | _ => .error .notSupported
  | .concat es =>
   let children ←  determineList ctx es
   pure (WidthTree.node children (children.map (·.width)).sum false)
  |.mult_concat ne ces =>
   let factor := (← expectBits (← evalConst ctx.consts ne)).norm.toNat
   let children ←  determineList ctx ces
   pure (WidthTree.node  children  (factor * (children.map (·.width)).sum) false)
  | .tf_call tfid aes =>
    let f ← ctx.funcs tfid
    let out_shape ← expectBits f.returnShape
    pure (WidthTree.node (← determineList ctx aes) (out_shape.width) out_shape.signed)
  | .system_tf_call tf aes =>
    match tf with
    | .clog2 =>
      match aes with
      | [elem] => pure (WidthTree.node [(← determine ctx elem)] sz_int32 true)
      | _ => .error .notSupported
    | .signed =>
      match aes with
      | [elem] =>
        let arg ← determine ctx elem
        pure (WidthTree.node [arg] arg.width true)
      | _ => .error .notSupported
    | .unsigned => match aes with
      | [elem] =>
        let arg ← determine ctx elem
        pure (WidthTree.node [arg] arg.width false)
      | _ => .error .notSupported
  | .cast sze e =>
    let w ← evalConst ctx.consts (exprToConst sze)
    let factor := (← expectBits w).norm.toNat
    let eTree ←  determine ctx e
    pure (WidthTree.node [eTree] factor eTree.signed)
  |.unary_op op e =>
  let oprnd ← determine ctx e
   match op with
   |.plus | .minus |.neg => pure (WidthTree.node [oprnd] oprnd.width oprnd.signed)
   |.not | .and |.nand |.or |.nor |.xor |.xnor => pure (WidthTree.node [oprnd] 1 false)
  |.inc_or_dec (.inc vid) | .inc_or_dec (.dec vid) => (determine ctx (.ident vid) )
  | .binary_op op le re =>
      let left ← determine ctx le
      let right ← determine ctx re
      match op with
      | .add |.sub |.mul |.div |.rem |.band |.bor |.bxor |.bxnor   =>
        pure  (WidthTree.node [left, right]  (max left.width right.width) (left.signed && right.signed))
      |.eq |.neq |.feq |.fneq |.weq |.wneq |.land |.lor |.lt |.le |.gt |.ge =>
        pure (WidthTree.node [left, right] 1 false)
      | .pow |.shr |.shl |.sar |.sal =>
        pure (WidthTree.node [left,right] left.width left.signed)
  |.cond ce te fe =>
   let j ← determine ctx te
   let k ← determine ctx fe
   pure (WidthTree.node [(← determine ctx ce), j,k] (max j.width k.width) (j.signed && k.signed))
  |.inside ie res => -- a binary output so it gets width one but children will be resized
   let res_lst ← determineList ctx res
   let ie_w ← determine ctx ie
   pure (WidthTree.node (ie_w::res_lst) 1 false)


def determineList (ctx: ModuleCtx)(expr_list: List expression) :  trsOk (List WidthTree) := do
 match expr_list with
 | [] => pure []
 | e ::es => pure ([ (←  determine ctx e)] ++ (←  determineList ctx es))

end

mutual
def propagate (ctx: ModuleCtx) (expr: expression) (wtree: WidthTree) (target: Nat) (ctxSigned: Bool): trsOk expression := do
 match expr with
 | .primary_literal _pl | .ident _name =>
    if wtree.width==0 then pure expr else
    let exprSignDet := if (!ctxSigned && wtree.signed) then .system_tf_call .unsigned [expr] else expr
    pure (.cast (.primary_literal (.number (.integral (.decimal (.unsigned target))))) exprSignDet)

 |.hierarchical_ident pe ce =>
    let pe_new ← match wtree with
     |.node [peTree] _ _ => propagate ctx pe peTree peTree.width peTree.signed
     | _ => .error .fatal
    if wtree.width==0 then pure (.hierarchical_ident pe_new ce) else
    let exprWidthDet := (expression.hierarchical_ident pe_new ce)
    let exprSignDet := if (!ctxSigned && wtree.signed ) then .system_tf_call .unsigned [exprWidthDet] else exprWidthDet
    pure (.cast (.primary_literal (.number (.integral (.decimal (.unsigned target))))) exprSignDet)

 |.select te se =>
  let te_new ← match wtree with
     |.node [teTree,_] _ _ => propagate ctx te teTree teTree.width teTree.signed
     | _ => .error .fatal
  let se_new ←  match wtree with
     |.node [_, seTree] _ _ => propagate ctx se seTree seTree.width seTree.signed
     | _ => .error .fatal
  if wtree.width==0 then pure (.select te_new se_new) else
  let exprWidthDet := (expression.select te_new se_new)
  let exprSignDet := if (!ctxSigned && wtree.signed ) then .system_tf_call .unsigned [exprWidthDet] else exprWidthDet
  pure (.cast (.primary_literal (.number (.integral (.decimal (.unsigned target))))) exprSignDet)

 |.select_const_range se lr rr =>
  let se_new ← match wtree with
     |.node [seTree,_,_] _ _ => propagate ctx se seTree seTree.width seTree.signed
     | _ => .error .fatal
  let lr_new ←  match wtree with
     |.node [_, lrTree, _] _ _ => propagate ctx lr lrTree lrTree.width lrTree.signed
     | _ => .error .fatal
  let rr_new ←  match wtree with
     |.node [_, _,rrTree] _ _ => propagate ctx rr rrTree rrTree.width rrTree.signed
     | _ => .error .fatal
  if wtree.width==0 then pure (.select_const_range se_new lr_new rr_new) else
  pure (.cast (.primary_literal (.number (.integral (.decimal (.unsigned target))))) (.select_const_range se_new lr_new rr_new))
 |.select_indexed_range_add se lr rr =>
    let se_new ← match wtree with
      |.node [seTree,_,_] _ _ => propagate ctx se seTree seTree.width seTree.signed
      | _ => .error .fatal
    let lr_new ←  match wtree with
      |.node [_, lrTree, _] _ _ => propagate ctx lr lrTree lrTree.width lrTree.signed
      | _ => .error .fatal
    let rr_new ←  match wtree with
      |.node [_, _,rrTree] _ _ => propagate ctx rr rrTree rrTree.width rrTree.signed
      | _ => .error .fatal
    if wtree.width==0 then pure (.select_indexed_range_add se_new lr_new rr_new) else
    pure (.cast (.primary_literal (.number (.integral (.decimal (.unsigned target))))) (.select_indexed_range_add se_new lr_new rr_new))

 |.select_indexed_range_sub se lr rr =>
    let se_new ← match wtree with
      |.node [seTree,_,_] _ _ => propagate ctx se seTree seTree.width seTree.signed
      | _ => .error .fatal
    let lr_new ←  match wtree with
      |.node [_, lrTree, _] _ _ => propagate ctx lr lrTree lrTree.width lrTree.signed
      | _ => .error .fatal
    let rr_new ←  match wtree with
      |.node [_, _,rrTree] _ _ => propagate ctx rr rrTree rrTree.width rrTree.signed
      | _ => .error .fatal
    if wtree.width==0 then pure (.select_indexed_range_sub se_new lr_new rr_new) else
    pure (.cast (.primary_literal (.number (.integral (.decimal (.unsigned target))))) (.select_indexed_range_sub se_new lr_new rr_new))
 | .concat es =>
   let children_wtrees := match wtree with
   | .node c _ _ => c
   let children_new ← propagateList ctx es children_wtrees (children_wtrees.map (·.width)) (children_wtrees.map (·.signed))
   pure (.cast (.primary_literal (.number (.integral (.decimal (.unsigned target))))) (.concat children_new))
 | .mult_concat ne es =>
   let children_wtrees := match wtree with
   | .node c _ _ => c
   let children_new ← propagateList ctx es children_wtrees (children_wtrees.map (·.width)) (children_wtrees.map (·.signed))
   pure (.cast (.primary_literal (.number (.integral (.decimal (.unsigned target))))) (.mult_concat ne children_new))
 | .tf_call tfid aes =>
  let f ← ctx.funcs tfid
  let children_wtrees := match wtree with
      | .node c _ _ => c
  let targets := List.zipWith max (f.inputShapes.map valWidth) (children_wtrees.map (·.width))
  let children_new ← propagateList ctx aes children_wtrees targets (children_wtrees.map (·.signed))
  let exprWidthDet := (expression.tf_call tfid children_new)
  let exprSignDet := if (!ctxSigned && wtree.signed ) then .system_tf_call .unsigned [exprWidthDet] else exprWidthDet
  pure (.cast (.primary_literal (.number (.integral (.decimal (.unsigned target))))) exprSignDet)
 | .system_tf_call tf aes =>
  let children_wtrees := match wtree with
      | .node c _ _ => c
  let targets :=  children_wtrees.map (·.width)
  let children_new ← propagateList ctx aes children_wtrees targets (children_wtrees.map (·.signed))
  let exprWidthDet := (expression.system_tf_call tf children_new)
  let exprSignDet := if (!ctxSigned && wtree.signed ) then .system_tf_call .unsigned [exprWidthDet] else exprWidthDet
  pure (.cast (.primary_literal (.number (.integral (.decimal (.unsigned target))))) exprSignDet)

 | .cast _sze e =>
  match wtree with
  | .node [eTree] w _ =>
    let exprWidthDet := (expression.cast (.primary_literal (.number (.integral (.decimal (.unsigned w))))) ( ← propagate ctx e eTree (max w eTree.width) eTree.signed))
    let exprSignDet := if (!ctxSigned && wtree.signed ) then .system_tf_call .unsigned [exprWidthDet] else exprWidthDet
    pure (.cast (.primary_literal (.number (.integral (.decimal (.unsigned target))))) exprSignDet)
  | _ => .error .fatal
 | .unary_op op e =>
  let child ←  match wtree with
             | .node [tr] _ _ => pure tr
             | _ => .error .fatal

  match op with
    | .plus | .minus | .neg  =>
    let child_ex ← propagate ctx e child target ctxSigned
    let exprWidthDet := (expression.unary_op op child_ex)
    let exprSignDet := if (!ctxSigned && wtree.signed ) then .system_tf_call .unsigned [exprWidthDet] else exprWidthDet
    pure (.cast (.primary_literal (.number (.integral (.decimal (.unsigned target))))) exprSignDet)
    | _ =>
    let child_ex ← propagate ctx e child child.width child.signed
    let exprWidthDet := (expression.unary_op op child_ex)
    let exprSignDet := if (!ctxSigned && wtree.signed ) then .system_tf_call .unsigned [exprWidthDet] else exprWidthDet
    pure (.cast (.primary_literal (.number (.integral (.decimal (.unsigned target))))) exprSignDet)
 |.inc_or_dec iod =>
   let exprWidthDet := (expression.inc_or_dec iod)
   let exprSignDet := if (!ctxSigned && wtree.signed ) then .system_tf_call .unsigned [exprWidthDet] else exprWidthDet
   pure (.cast (.primary_literal (.number (.integral (.decimal (.unsigned target))))) exprSignDet)
 | .binary_op op le re =>
  let (left,right) ←  match wtree with
                    | .node [left,right] _ _ => pure (left, right)
                    | _ => .error .fatal
    match op with
    | .add |.sub | .mul | .div | .rem | .band | .bor | .bxor | .bxnor   =>
            pure ( .binary_op op ( ← propagate ctx le left target ctxSigned) (←  propagate ctx re right target ctxSigned))
    | .eq | .neq | .feq | .fneq | .weq | .wneq | .lt | .le | .gt | .ge =>
            let w := max left.width right.width
            let signed := left.signed && right.signed
            pure (.cast (.primary_literal (.number (.integral (.decimal (.unsigned target))))) (.binary_op op ( ← propagate ctx le left w signed) (←  propagate ctx re right w signed)))
    | .land | .lor => pure (.cast (.primary_literal (.number (.integral (.decimal (.unsigned target))))) (.binary_op op ( ← propagate ctx le left left.width left.signed) (←  propagate ctx re right right.width right.signed)))
    | .pow | .shr | .shl | .sar | .sal =>
          pure (.cast (.primary_literal (.number (.integral (.decimal (.unsigned target))))) (.binary_op op ( ← propagate ctx le left target ctxSigned) (←  propagate ctx re right right.width right.signed)))

 | .cond ce te fe =>
    let (iTree, jTree, kTree) ←  match wtree with
                    | .node [i,j,k] _ _ => pure (i, j, k)
                    | _ => .error .fatal

    pure (.cond (← propagate ctx ce iTree iTree.width iTree.signed) (← propagate ctx te jTree target ctxSigned) (← propagate ctx fe kTree target ctxSigned) )
 | .inside ie res =>
   let (ieTree,children_wtrees) ←  match wtree with
                                   | .node (iTree::resTree) _ _ => pure (iTree, resTree)
                                   | _ => .error .fatal

   let w:= List.foldl max ieTree.width (children_wtrees.map (·.width))
   let shared_signed := children_wtrees.all (·.signed) && ieTree.signed
   let children_new ← propagateList ctx res children_wtrees (List.replicate children_wtrees.length w) (List.replicate children_wtrees.length shared_signed)
   let ie_new ← propagate ctx ie ieTree w shared_signed
   pure  (.cast (.primary_literal (.number (.integral (.decimal (.unsigned target))))) (.inside ie_new children_new))

def propagateList (ctx: ModuleCtx) (expr: List expression) (wtrees: List WidthTree) (targets: List Nat) (ctxSigned: List Bool): trsOk (List expression) := do
 match expr,wtrees,targets, ctxSigned with
 |[],[],[], [] => pure []
 | exp::rest, tree::restr, tgt::rem_tgts, s::signRest => pure ((← propagate ctx exp tree tgt s)::(← propagateList ctx rest restr rem_tgts signRest))
 | _, _, _, _ => .error .fatal

end
def evalConstCore (e: constant_expression) (c: Consts) (destWidth: Nat): trsOk Value := do
 let expr_version := constToExpr e
 let ctx : ModuleCtx := { tdefs := HMap.empty, decls := State.empty, funcs := fmapEmpty, consts := c }
 let det_tree ← determine ctx expr_version
 evalConst c (exprToConst (← propagate ctx expr_version det_tree (max destWidth det_tree.width) det_tree.signed))

def intLit (v: Int): expression :=
 if v < 0 then
  .unary_op .minus (.primary_literal (.number (.integral (.decimal (.unsigned (-v).toNat)))))
 else
 (.primary_literal (.number (.integral (.decimal (.unsigned v.toNat)))))

def sizeToLit  (ce : constant_expression) (c : Consts): trsOk constant_expression := do
 let evaled ← evalConstCore ce c 0
 let num := (← expectBits evaled).norm
 pure (exprToConst (intLit num))

mutual
def literalizeConst (c_expr : constant_expression) (c : Consts) : trsOk constant_expression := do
 match c_expr with
  | .primary_literal _ | .ident _ | .inc_or_dec _ => pure c_expr
  | .hierarchical_ident pe ce => pure (.hierarchical_ident (← literalizeConst pe c) (← literalizeConst ce c))
  | .select te se => pure (.select (← literalizeConst te c) (← literalizeConst se c))
  | .select_const_range se lr rr => pure (.select_const_range (← literalizeConst se c) (← sizeToLit (← literalizeConst lr c) c) (← sizeToLit (← literalizeConst rr c) c))
  | .select_indexed_range_add se lr rr => pure (.select_indexed_range_add (← literalizeConst se c) (← literalizeConst lr c) (← sizeToLit (← literalizeConst rr c) c))
  | .select_indexed_range_sub se lr rr => pure (.select_indexed_range_sub (← literalizeConst se c ) (← literalizeConst lr c) (← sizeToLit (← literalizeConst rr c) c))
  | .concat es => pure (.concat (← literalizeConstList es c))
  | .mult_concat ne ces => pure (.mult_concat  (← sizeToLit (← literalizeConst ne c) c) (← literalizeConstList ces c))
  | .tf_call tfid aes => pure (.tf_call tfid (← literalizeConstList aes c))
  | .system_tf_call tf aes => pure (.system_tf_call tf (← literalizeConstList aes c))
  | .cast sze e => pure (.cast (← sizeToLit (← literalizeConst sze c) c) (← literalizeConst e c))
  | .unary_op op e => pure (.unary_op op (← literalizeConst e c))
  | .binary_op op le re => pure (.binary_op op (← literalizeConst le c) (← literalizeConst re c))
  | .cond ce te fe => pure (.cond (← literalizeConst ce c) (← literalizeConst te c) (← literalizeConst fe c))
  | .inside ie res => pure (.inside (← literalizeConst ie c) (← literalizeConstList res c))

def literalizeConstList (ces : List constant_expression) (c : Consts) : trsOk (List constant_expression) := do
  match ces with
    | [] => pure []
    | e :: es => pure ((← literalizeConst e c) :: (← literalizeConstList es c))
end

def literalizeExpr (e: expression) (c: Consts): trsOk (expression) := do
  let ce := exprToConst e
  let literalized ← literalizeConst ce c
  pure (constToExpr literalized)


def evalConstW (e: constant_expression) (c: Consts) (destWidth: Nat): trsOk Value := do
 let sizes_literalized ← literalizeConst e c
 let evaled ← evalConstCore sizes_literalized c destWidth
 pure (evaled)


-- ## Declaration size helpers

def evalPackedDimWidth (consts : Consts) : dim → trsOk Nat
  | .range lr rr => do
      let lv ← evalConstW  lr consts 0
      let rv ← evalConstW  rr consts 0
      let lsz ← expectBits lv
      let rsz ← expectBits rv
      let left := lsz.norm
      let right := rsz.norm
      let distance := if left < right then right - left else left - right
      pure (distance.toNat + 1)
  | .one de => do
      let dv ← evalConstW de consts 0
      let dsz ← expectBits dv
      if dsz.norm > 0 then pure dsz.norm.toNat else .error .fatal

def evalPackedDimsWidth (consts : Consts) : packed_dims → trsOk (Option Nat)
  | .nil => pure none
  | .one pd => do
      let width ← evalPackedDimWidth consts pd
      pure (some width)
  | .cons pd pds => do
      let width ← evalPackedDimWidth consts pd
      let rest ← evalPackedDimsWidth consts pds
      pure (some (width * rest.getD 1))

/- Evaluate packed dimensions to get the declaration size (as HMap).
   Returns `none` for `nil`. -/
def evalPackedDims (consts : Consts) (pds : packed_dims) : trsOk (Option Value) := do
  let width ← evalPackedDimsWidth consts pds
  pure (width.map fun w => HMap.bits (SZ.mk' 0 w false))

-- Get the default value for a data type.
def declDataType (tdefs : TDefs) (consts : Consts) : data_type → trsOk Value
  | .int_vec _ pds => do
      let ov ← evalPackedDims consts pds
      match ov with
      | some v => pure v
      | none => pure (HMap.bits (SZ.mk' 0 1 false))
  | .int_atom .byte => pure (HMap.bits (SZ.mk' 0 8 false))
  | .int_atom .short_int => pure (HMap.bits (SZ.mk' 0 16 true))
  | .int_atom .int => pure (HMap.bits (SZ.mk' 0 32 true))
  | .int_atom .long_int => pure (HMap.bits (SZ.mk' 0 64 true))
  | .int_atom .integer => pure (HMap.bits (SZ.mk' 0 32 true))
  | .int_atom .time => pure (HMap.bits (SZ.mk' 0 64 false))
  | .enum base _ _ => declDataType tdefs consts base
  | .named tid _ =>
      match haccessO tdefs tid with
      | some v => pure v
      | none => .error .undeclared

def declDataTypeOrImplicit (tdefs : TDefs) (consts : Consts) : data_type_or_implicit → trsOk Value
  | .data dt => declDataType tdefs consts dt
  | .implicit pds => do
      let ov ← evalPackedDims consts pds
      match ov with
      | some v => pure v
      | none => pure (HMap.bits (SZ.mk' 0 1 false))

def declPortType (consts : Consts) : port_type → trsOk Value
  | .port _ pds => do
      let ov ← evalPackedDims consts pds
      match ov with
      | some v => pure v
      | none => pure (HMap.bits (SZ.mk' 0 1 false))

-- Length of one unpacked dimension: `[n]` → n, `[l:r]` → |l-r|+1.
def unpackedDimLen (consts : Consts) : dim → trsOk Nat
  | .one de => do let dv ← evalConstW de consts 0; pure (hbits dv).norm.toNat
  | .range lr rr => do
      let lv ← evalConstW lr consts 0
      let rv ← evalConstW rr consts 0
      pure (((hbits lv).norm - (hbits rv).norm).natAbs + 1 : Nat)

-- Wrap an element shape in nested arrays, one level per unpacked dimension.
def wrapUnpacked (consts : Consts) (elem : Value) : packed_dims → trsOk Value
  | .nil => pure elem
  | .one d => do
      let n ← unpackedDimLen consts d
      pure (harray (List.replicate n elem))
  | .cons d ds => do
      let inner ← wrapUnpacked consts elem ds
      let n ← unpackedDimLen consts d
      pure (harray (List.replicate n inner))

-- ## Parameter value collection

def paramValue (consts : Consts) (dti : data_type_or_implicit) (ce : constant_expression) :
    trsOk Value := do
  match dti with
  | .implicit .nil => pure (← evalConstW ce consts 0)
  | _ => do
      let dv ← declDataTypeOrImplicit HMap.empty consts dti
      let v ← evalConstW ce consts (valWidth dv)
      let dsz ← expectBits dv
      let vsz ← expectBits v
      pure (HMap.bits (SZ.castD dsz vsz))

def evalParamAssignsInto (consts : Consts) (dti : data_type_or_implicit) :
    param_assigns → trsOk Consts
  | .one (.param pid (.min_typ_max ce)) => do
      let v ← paramValue consts dti ce
      pure (hupds consts (HMap.str [(pid, v)]))
  | .cons (.param pid (.min_typ_max ce)) rest => do
      let v ← paramValue consts dti ce
      evalParamAssignsInto (hupds consts (HMap.str [(pid, v)])) dti rest

def collectParamPortValues (consts : Consts) : param_ports → trsOk Consts
  | .nil => pure consts
  | .one (.data dti pas) => evalParamAssignsInto consts dti pas
  | .cons (.data dti pas) rest => do
      let consts' ← evalParamAssignsInto consts dti pas
      collectParamPortValues consts' rest

def collectParamsPkgGenItemDecl (consts : Consts) : pkg_gen_item_decl → trsOk Consts
  | .param (.data dti pas) => evalParamAssignsInto consts dti pas
  | .local_param (.local dti pas) => evalParamAssignsInto consts dti pas
  | _ => pure consts

def collectParamsModuleCommonItem (consts : Consts) : module_common_item → trsOk Consts
  | .decl (.pkg pgid) => collectParamsPkgGenItemDecl consts pgid
  | _ => pure consts

def collectParamsModuleOrGenerateItem (consts : Consts) : module_or_generate_item → trsOk Consts
  | .common ci => collectParamsModuleCommonItem consts ci
  | .ins _ => pure consts

mutual
def collectParamsGenerateModuleItem (consts : Consts) : generate_module_item → trsOk Consts
  | .module mogi => collectParamsModuleOrGenerateItem consts mogi
  | .cond ce tgmi fgmi => do
      let cv ← evalConstW ce consts 0
      let csz ← expectBits cv
      if csz.isZero then
        match fgmi with
        | none => pure consts
        | some fgmi' => collectParamsGenerateModuleItem consts fgmi'
      else
        collectParamsGenerateModuleItem consts tgmi
  | .block gmis => collectParamsGenerateModuleItemList consts gmis

def collectParamsGenerateModuleItemList (consts : Consts) : List generate_module_item → trsOk Consts
  | [] => pure consts
  | gmi :: rest => do
      let consts' ← collectParamsGenerateModuleItem consts gmi
      collectParamsGenerateModuleItemList consts' rest
end

def collectParamsNonPortModuleItem (consts : Consts) : non_port_module_item → trsOk Consts
  | .generated_module_ins (.generated gmi) => collectParamsGenerateModuleItem consts gmi
  | .module_or_generate_item mogi => collectParamsModuleOrGenerateItem consts mogi

def collectParamValuesItem (consts : Consts) : module_item → trsOk Consts
  | .port_decl _ => pure consts
  | .non_port np => collectParamsNonPortModuleItem consts np

def collectParamValues : module_items → Consts → trsOk Consts
  | .one mi, consts => collectParamValuesItem consts mi
  | .cons mi mis, consts => do
      let consts' ← collectParamValuesItem consts mi
      collectParamValues mis consts'

def computeConsts : module_decl → trsOk Consts
  | .ansi _ pps _ mitems => do
      let env0 ← collectParamPortValues (.str []) pps
      collectParamValues mitems env0

-- ## Typedef / enum collection — build TDefs and fold enum names into Consts

-- Assign sequential values to enum variants (auto-increment, explicit `= val` resets).
def collectEnumVariants (consts : Consts) (baseW : Nat) (baseS : Bool) :
    Int → List enum_variant → trsOk Consts
  | _, [] => pure consts
  | next, (.var name oval) :: rest => do
      let val ← match oval with
        | some ce => do let v ← evalConstW ce consts baseW; pure (hbits v).norm
        | none => pure next
      let cv := HMap.bits (SZ.mk' val baseW baseS)
      collectEnumVariants (hupds consts (HMap.str [(name, cv)])) baseW baseS (val + 1) rest

def collectTDefsTypeDecl (tdefs : TDefs) (consts : Consts) : type_decl → trsOk (TDefs × Consts)
  | .typedef dt tid => do
      let shape ← declDataType tdefs consts dt
      let tdefs' := hupds tdefs (HMap.str [(tid, shape)])
      let consts' ← match dt with
        | .enum base variants _ => do
            let bsz := hbits (← declDataType tdefs consts base)
            collectEnumVariants consts bsz.width bsz.signed 0 variants
        | _ => pure consts
      pure (tdefs', consts')

def collectTDefsPkgGenItemDecl (tdefs : TDefs) (consts : Consts) :
    pkg_gen_item_decl → trsOk (TDefs × Consts)
  | .data (.type_decl td) => collectTDefsTypeDecl tdefs consts td
  | _ => pure (tdefs, consts)

def collectTDefsModuleCommonItem (tdefs : TDefs) (consts : Consts) :
    module_common_item → trsOk (TDefs × Consts)
  | .decl (.pkg pgid) => collectTDefsPkgGenItemDecl tdefs consts pgid
  | _ => pure (tdefs, consts)

def collectTDefsModuleOrGenerateItem (tdefs : TDefs) (consts : Consts) :
    module_or_generate_item → trsOk (TDefs × Consts)
  | .common ci => collectTDefsModuleCommonItem tdefs consts ci
  | .ins _ => pure (tdefs, consts)

mutual
def collectTDefsGenerateModuleItem (tdefs : TDefs) (consts : Consts) :
    generate_module_item → trsOk (TDefs × Consts)
  | .module mogi => collectTDefsModuleOrGenerateItem tdefs consts mogi
  | .cond _ tgmi fgmi => do
      let (tdefs', consts') ← collectTDefsGenerateModuleItem tdefs consts tgmi
      match fgmi with
      | none => pure (tdefs', consts')
      | some fgmi' => collectTDefsGenerateModuleItem tdefs' consts' fgmi'
  | .block gmis => collectTDefsGenerateModuleItemList tdefs consts gmis

def collectTDefsGenerateModuleItemList (tdefs : TDefs) (consts : Consts) :
    List generate_module_item → trsOk (TDefs × Consts)
  | [] => pure (tdefs, consts)
  | gmi :: rest => do
      let (tdefs', consts') ← collectTDefsGenerateModuleItem tdefs consts gmi
      collectTDefsGenerateModuleItemList tdefs' consts' rest
end

def collectTDefsNonPortModuleItem (tdefs : TDefs) (consts : Consts) :
    non_port_module_item → trsOk (TDefs × Consts)
  | .generated_module_ins (.generated gmi) => collectTDefsGenerateModuleItem tdefs consts gmi
  | .module_or_generate_item mogi => collectTDefsModuleOrGenerateItem tdefs consts mogi

def collectTDefsItem (tdefs : TDefs) (consts : Consts) : module_item → trsOk (TDefs × Consts)
  | .port_decl _ => pure (tdefs, consts)
  | .non_port np => collectTDefsNonPortModuleItem tdefs consts np

def collectTDefs : module_items → TDefs → Consts → trsOk (TDefs × Consts)
  | .one mi, tdefs, consts => collectTDefsItem tdefs consts mi
  | .cons mi mis, tdefs, consts => do
      let (tdefs', consts') ← collectTDefsItem tdefs consts mi
      collectTDefs mis tdefs' consts'

def computeTDefs (consts : Consts) : module_decl → trsOk (TDefs × Consts)
  | .ansi _ _ _ mitems => collectTDefs mitems HMap.empty consts

-- ## declfind / wfind — looking up declarations and values

-- Find the path to a variable in declarations.
def declfind (decls : Decls) (vid : VId) : trsOk HPath :=
  liftOption (hpos vid decls.fields) .undeclared

def declValue (decls : Decls) (path : HPath) : trsOk Value :=
  liftOption (decls.find? path) .undeclared

-- Find a variable value: look in nw, then ifw, then consts.
def wfind (ctx : ModuleCtx) (ifw : IFW) (nw : NW) (vid : VId) : trsOk Value :=
  match declfind ctx.decls vid with
  | .ok p =>
      match findState2 p nw ifw with
      | some v => pure v
      | none =>
          if ctx.localNames.contains vid then .error .undriven
          else match haccessO ctx.consts vid with
            | some v => pure v
            | none => .error .undriven
  | .error _ =>
      match haccessO ctx.consts vid with
      | some v => pure v
      | none => .error .undeclared






-- ## Expression evaluation (mutual recursion with evalExprList and lvposfind)


mutual

def evalExpr (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (nw : NW) : expression → trsOk Value
  | .primary_literal pl => pure (evalPriLiteral pl)
  | .ident vid => wfind ctx ifw nw vid
  | .hierarchical_ident pe ce => do
      let pv ← evalExpr ctx cpos ifw nw pe
      let cvid ← getAccessVid ce
      pure (haccess pv cvid)
  | .select te se => do
      let tv ← evalExpr ctx cpos ifw nw te
      let sv ← evalExpr ctx cpos ifw nw se
      let ssz ← expectBits sv
      pure (hselect tv ssz)
  | .select_const_range se lr rr => do
      let sv ← evalExpr ctx cpos ifw nw se
      let lv ← evalExpr ctx cpos ifw nw lr
      let rv ← evalExpr ctx cpos ifw nw rr
      let lsz ← expectBits lv
      let rsz ← expectBits rv
      pure (hrange sv lsz rsz)
  | .select_indexed_range_add se lr rr => do
      let sv ← evalExpr ctx cpos ifw nw se
      let lv ← evalExpr ctx cpos ifw nw lr
      let rv ← evalExpr ctx cpos ifw nw rr
      let lsz ← expectBits lv
      let rsz ← expectBits rv
      let hi := SZ.bAdd lsz (SZ.bSub rsz (SZ.mk' 1 rsz.width false))
      pure (hrange sv hi lsz)
  | .select_indexed_range_sub se lr rr => do
      let sv ← evalExpr ctx cpos ifw nw se
      let lv ← evalExpr ctx cpos ifw nw lr
      let rv ← evalExpr ctx cpos ifw nw rr
      let lsz ← expectBits lv
      let rsz ← expectBits rv
      let lo := SZ.bSub lsz (SZ.bSub rsz (SZ.mk' 1 rsz.width false))
      pure (hrange sv lsz lo)
  | .concat es => do
      let vs ← evalExprList ctx cpos ifw nw es
      let szs ← expectBitsList vs
      pure (HMap.bits (SZ.concat szs))
  | .mult_concat ne ces => do
      let nv ← evalConst ctx.consts ne
      let cvs ← evalExprList ctx cpos ifw nw ces
      let nsz ← expectBits nv
      let cszs ← expectBitsList cvs
      let count := nsz.norm.toNat
      pure (HMap.bits (SZ.rep count (SZ.concat cszs)))
  | .tf_call tfid aes => do
      let f ← ctx.funcs tfid
      let avs ← evalExprList ctx cpos ifw nw aes
      if f.inputVids.length != avs.length then .error .notSupported
      else
        let inputState := buildFInputState f.inputVids avs
        -- Functions use their own lexical scope, not the caller's locals.
        let functionIfw := (ifw.without ctx.localNames).merge ctx.hidden
        f.func [tfid] functionIfw (nw.without ctx.localNames) inputState
  | .system_tf_call .signed aes =>
      match aes with
      | [ae] => do
          let av ← evalExpr ctx cpos ifw nw ae
          let asz ← expectBits av
          pure (HMap.bits asz.toSigned)
      | _ => .error .notSupported
  | .system_tf_call .unsigned aes =>
      match aes with
      | [ae] => do
          let av ← evalExpr ctx cpos ifw nw ae
          let asz ← expectBits av
          pure (HMap.bits asz.toUnsigned)
      | _ => .error .notSupported
  | .system_tf_call .clog2 aes =>
      match aes with
      | [ae] => do
          let av ← evalExpr ctx cpos ifw nw ae
          pure (HMap.bits (SZ.mk' (clog2Nat (hbits av).norm.toNat) sz_int32 true))
      | _ => .error .notSupported
  | .cast sze e => do
      let szv ← evalExpr ctx cpos ifw nw sze
      let ev ← evalExpr ctx cpos ifw nw e
      let sz ← expectBits szv
      let esz ← expectBits ev
      pure (HMap.bits (SZ.castV sz esz))
  | .unary_op op e => do
      let ev ← evalExpr ctx cpos ifw nw e
      let esz ← expectBits ev
      pure (HMap.bits (uniOpFunc op esz))
  | .inc_or_dec (.inc vid) => do
      let v ← wfind ctx ifw nw vid
      let vsz ← expectBits v
      pure (HMap.bits (SZ.bAdd vsz (SZ.mk' 1 vsz.width false)))
  | .inc_or_dec (.dec vid) => do
      let v ← wfind ctx ifw nw vid
      let vsz ← expectBits v
      pure (HMap.bits (SZ.bSub vsz (SZ.mk' 1 vsz.width false)))
  | .binary_op op le re => do
      let lv ← evalExpr ctx cpos ifw nw le
      let rv ← evalExpr ctx cpos ifw nw re
      let lsz ← expectBits lv
      let rsz ← expectBits rv
      pure (HMap.bits (binOpFunc op lsz rsz))
  | .cond ce te fe => do
      let cv ← evalExpr ctx cpos ifw nw ce
      let tv ← evalExpr ctx cpos ifw nw te
      let fv ← evalExpr ctx cpos ifw nw fe
      let csz ← expectBits cv
      if csz.isZero then pure fv else pure tv
  | .inside ie res => do
      let iv ← evalExpr ctx cpos ifw nw ie
      let rvs ← evalExprList ctx cpos ifw nw res
      let isz ← expectBits iv
      let rszs ← expectBitsList rvs
      let isMatch := rszs.any (SZ.equiv isz)
      if isMatch
        then pure (HMap.bits (SZ.mk' 1 1 false))
        else pure (HMap.bits (SZ.mk' 0 1 false))

def evalExprList (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (nw : NW) : List expression → trsOk (List Value)
  | [] => .ok []
  | e :: es => do
      let v ← evalExpr ctx cpos ifw nw e
      let vs ← evalExprList ctx cpos ifw nw es
      pure (v :: vs)

def lvposfind (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (nw : NW) : expression → trsOk HPath
  | .ident vid => declfind ctx.decls vid
  | .hierarchical_ident pe ce => do
      let pp ← lvposfind ctx cpos ifw nw pe
      let cvid ← getAccessVid ce
      pure (pp ++ [HElt.vid cvid])
  | .select te se => do
      let pp ← lvposfind ctx cpos ifw nw te
      let sv ← evalExpr ctx cpos ifw nw se
      let ssz ← expectBits sv
      pure (pp ++ [HElt.ind ssz])
  | _ => .error .notSupported

end

private def localShape (ctx : ModuleCtx) (declaration : procedural_decl) : trsOk Value := do
  match declaration.dtype with
  | .int_vec .logic _ | .int_vec .bit _ => pure ()
  | _ => .error .notSupported
  let bits ← expectBits (← declDataType ctx.tdefs ctx.consts declaration.dtype)
  pure (.bits { bits with signed := declaration.signed })

-- Local declarations are a prefix of a sequential block, never statements
-- that can be entered conditionally without their enclosing lexical frame.
private def blockDecls : List statement_item → List procedural_decl × List statement_item
  | .local_decl declaration :: rest =>
      let (declarations, body) := blockDecls rest
      (declaration :: declarations, body)
  | body => ([], body)

def declsVAnsiPortDecl (tdefs : TDefs) (consts : Consts) : ansi_port_decl → trsOk (List (VId × Value))
  | .net (some (.net _ pt)) pid => do
      let dv ← declPortType consts pt
      pure [(pid, dv)]
  | .net none pid => pure [(pid, HMap.bits (SZ.mk' 0 1 false))]
  | .var (some (.var _ dt)) pid vd => do
      let basev ← declDataType tdefs consts dt
      let dv ← wrapUnpacked consts basev vd
      pure [(pid, dv)]
  | .var none pid _ => pure [(pid, HMap.bits (SZ.mk' 0 1 false))]

def declsVAnsiPortDecls (tdefs : TDefs) (consts : Consts) : ansi_port_decls → trsOk (List (VId × Value))
  | .nil => pure []
  | .one apd => declsVAnsiPortDecl tdefs consts apd
  | .cons apd apds => do
      let d ← declsVAnsiPortDecl tdefs consts apd
      let rest ← declsVAnsiPortDecls tdefs consts apds
      pure (d ++ rest)




-- ## Applying the width determination pass
/-
Here the module is traversed and the width pass is called
-/
def widthAssignLike (ctx: ModuleCtx) (destWidth: Nat) (e: expression): trsOk expression := do
 let literalized_expressions ← literalizeExpr e ctx.consts -- any size constants are evaluated and written back as literals
 let wtree ← determine ctx literalized_expressions
 let tgt := max destWidth wtree.width
 propagate ctx literalized_expressions wtree tgt wtree.signed

def widthLvalueAssignLike (ctx: ModuleCtx) (e: expression): trsOk expression := do
  match e with
  | .ident _ => pure e
  | .select te se => pure (.select (← widthLvalueAssignLike ctx te) (← widthAssignLike ctx 0 se)) -- se goes through determine and propagate. index on lhs is self determined
  | .hierarchical_ident pe ce => pure (.hierarchical_ident  (← widthLvalueAssignLike ctx pe)  ce)
  | _ => .error .notSupported -- remaining not allowed on lhs

def widthAssign (ctx: ModuleCtx) (a: assign): trsOk assign := do
  match a with
  | .net lv e =>
   let left ← widthLvalueAssignLike ctx lv
   let w ← determine ctx lv -- the lhs part of the determine tree. done separately becase assign is split into lv and e so this function is the top node in a sense. no propagate because this is lhs not in a computation
   pure (.net left  (←  widthAssignLike ctx w.width e))

def widthAssigns (ctx: ModuleCtx) (a: assigns): trsOk assigns := do
  match a with
  | .one na => pure (.one (← widthAssign ctx na))
  | .cons na nas => pure (.cons  (← widthAssign ctx na) (← widthAssigns ctx nas))

-- finds the width of the case (only the  expression inside case and matching values )
def caseWidth (ctx: ModuleCtx) (ce: expression) (css: List (case_item statement_item)): trsOk Nat := do
  match css with
  | [] => pure ((← determine ctx ce).width)
  | (.case expr _st)::cssi => pure ( max ((← determine ctx expr).width)  (← caseWidth ctx ce cssi))
  | (.default _st)::cssi => caseWidth ctx ce cssi



def widthForStep (ctx: ModuleCtx) (step: for_step): trsOk for_step := do
  match step with
  | .op_assign (.op lv aop e) =>
      match aop with
      | .shl | .shr | .sal | .sar => pure (.op_assign (.op (← widthLvalueAssignLike ctx lv ) aop  (← widthAssignLike ctx 0 e)))  -- shift number not involved in width
      | _ =>
       let lw := (← determine ctx lv).width
       pure (.op_assign (.op (← widthLvalueAssignLike ctx lv) aop (← widthAssignLike ctx lw e)))
  | .inc_or_dec _ => pure step


mutual
def widthCaseItems (ctx: ModuleCtx) (returnWidth:Nat) (w: Nat) (css: List (case_item statement_item)): trsOk (List (case_item statement_item)) := do
  match css with
  | [] => pure []
  | (.case expr st)::cssi => pure ((.case (← widthAssignLike ctx w expr) (← widthStatementItem ctx returnWidth st)) ::  (← widthCaseItems ctx returnWidth w cssi))
  | (.default st)::cssi => pure ((.default (← widthStatementItem ctx returnWidth st)) ::  (← widthCaseItems ctx returnWidth w cssi))

def widthStatementItem (ctx: ModuleCtx) (returnWidth: Nat) (si: statement_item): trsOk statement_item := do
  match si with
  | .blocking_assign_normal vlv e  =>
   let left ← widthLvalueAssignLike ctx vlv  -- fix width of  sub expressions insde lhs
   let w ← determine ctx vlv
   let right ← widthAssignLike ctx w.width e
   pure (.blocking_assign_normal left right)
  |.nonblocking_assign vlv e =>
   let left ← widthLvalueAssignLike ctx vlv
   let w ← determine ctx vlv
   let right ← widthAssignLike ctx w.width e
   pure (.nonblocking_assign left right)
  | .case cty ce  css =>
    let cw ← caseWidth ctx ce css -- a case width is from the ce and all the css case_item
    pure (.case cty (← widthAssignLike ctx cw ce) (← widthCaseItems ctx returnWidth cw css))
  | .cond cp ts fs =>
    let cp_expr ← widthAssignLike ctx 0 cp
    let stat_i ←   match ts with
            | none => pure none
            | some x => pure (some (← widthStatementItem ctx returnWidth x))
    let fs_new ← match fs with
                 | none => pure none
                 | some none => pure (some none)
                 | some (some stmt_i) => pure (some (some (← widthStatementItem ctx returnWidth stmt_i)))

    pure (.cond cp_expr stat_i fs_new )

  | .forever s => pure (.forever (← widthStatementItem ctx returnWidth s))
  | .repeat re s => pure (.repeat (← widthAssignLike ctx 0 re) (← widthStatementItem ctx returnWidth s))
  | .while re s => pure (.while (← widthAssignLike ctx 0 re) (← widthStatementItem ctx returnWidth s))
  | .for (.var_assigns vas) ce step stmti => pure (.for (.var_assigns (← widthAssigns ctx vas))
                                                   (← widthAssignLike ctx 0 ce) (← widthForStep ctx step) (← widthStatementItem ctx returnWidth stmti))
  | .do_while s re => pure (.do_while  (← widthStatementItem ctx returnWidth s) (← widthAssignLike ctx 0 re))
  | .return re => pure (.return (← widthAssignLike ctx returnWidth re))
  | .proc_timing_control ptc s => pure (.proc_timing_control ptc (← widthStatementItem ctx returnWidth s))
  | .seq_block stis =>
    let (declarations, _body) := blockDecls stis
    let shapes ← sfListMap (fun declaration => do
                        pure (declaration.name, ← localShape ctx declaration)) declarations
    let localCtx : ModuleCtx := { ctx with decls := ctx.decls.merge ⟨shapes⟩}

    pure (.seq_block (← widthStatementItemList localCtx returnWidth stis))
  | .local_decl decl =>
    match decl.initializer with
    | none => pure (.local_decl decl)
    | some x =>
      let dest ← determine ctx (.ident decl.name)
      pure (.local_decl ({decl with initializer := (← widthAssignLike ctx dest.width x )}))

  | .skip => pure si


def widthStatementItemList (ctx: ModuleCtx) (returnWidth: Nat) (list_si: List statement_item): trsOk (List statement_item) := do
  match list_si with
  | [] => pure []
  | s::sis => pure ((← widthStatementItem ctx returnWidth s) :: (← widthStatementItemList ctx returnWidth sis))

end

def widthVarDeclAssign (ctx: ModuleCtx) (var: var_decl_assign): trsOk var_decl_assign := do
  match var with
  | .var _vid _vd none => pure var
  | .var vid vd (some x) =>
   let left ← determine ctx (.ident vid)
   let w ← widthAssignLike ctx left.width x
   pure (.var vid vd (some w))

def widthVarDeclAssigns (ctx: ModuleCtx) (vdas: var_decl_assigns): trsOk var_decl_assigns := do
 match vdas with
 | .one pa => pure (.one (← widthVarDeclAssign ctx pa))
 | .cons pa pas => pure (.cons (← widthVarDeclAssign ctx pa) (← widthVarDeclAssigns ctx pas))

def widthDataDecl (ctx: ModuleCtx) (dd: data_decl): trsOk data_decl := do
  match dd with
  | .var_decl (.var dt vdas) => pure (.var_decl (.var dt (← widthVarDeclAssigns ctx vdas) ))
  | .type_decl _ =>  pure dd

def widthNetDeclAssign  (ctx: ModuleCtx) (net: net_decl_assign): trsOk net_decl_assign := do
  match net with
  | .net nid none  => pure (.net nid none)
  | .net nid (some x) =>
   let left ← determine ctx (.ident nid)
   let w ← widthAssignLike ctx left.width x
   pure (.net nid (some w))

def widthNetDeclAssigns (ctx: ModuleCtx) (nda: net_decl_assigns): trsOk net_decl_assigns := do
  match nda with
  | .one pa => pure (.one (← widthNetDeclAssign ctx pa) )
  | .cons pa pas => pure (.cons  (← widthNetDeclAssign ctx pa) (← widthNetDeclAssigns ctx pas))

def widthPkgGenItemDecl (ctx: ModuleCtx) (pk: pkg_gen_item_decl): trsOk pkg_gen_item_decl := do
  match pk with
  | .net (.net nt pd nda) => pure (.net (.net nt pd (← widthNetDeclAssigns ctx nda) ))
  | .data dd => pure (.data (← widthDataDecl ctx dd))
  | .task (.task tis (.stmt si)) => pure (.task (.task tis  (.stmt (← widthStatementItem ctx 0 si)) ))
  | .func (.func dti fid ports (.stmt si)) =>

    let portDecls ← declsVAnsiPortDecls ctx.tdefs ctx.consts ports
    let returnDecl ← declDataTypeOrImplicit ctx.tdefs ctx.consts dti
    let funcCtx := { ctx with decls := ctx.decls.merge ⟨portDecls⟩}
    pure (.func (.func dti fid ports (.stmt (← widthStatementItem funcCtx (valWidth returnDecl) si))))
  | .param (.data _dti _pas) => pure pk -- constant expressions  handled in evalConst
  | .local_param (.local _dti _pas) => pure pk

def widthModCommItem (ctx: ModuleCtx) (ci: module_common_item): trsOk module_common_item := do
  match ci with
  | .decl (.pkg pd) => pure (.decl   (.pkg (← widthPkgGenItemDecl ctx pd)))
  | .assert _ => pure ci
  | .cont_assign (.net nas ) => pure (.cont_assign  (.net   (← widthAssigns ctx nas)) )
  | .initial _ => pure ci
  | .always ak (.stmt si) => pure (.always ak  (.stmt (← widthStatementItem ctx 0 si)))




def widthModOrGen (ctx: ModuleCtx) (mgi: module_or_generate_item): trsOk module_or_generate_item := do
 match mgi with
  | .ins (.module _mid _pva (.hier _iid (.named _npcs))) =>
    pure mgi
  | .common ci => pure ( .common (← widthModCommItem ctx ci))

mutual
def widthGenModItem (ctx: ModuleCtx) (gmi: generate_module_item): trsOk generate_module_item := do
  match gmi with
  | .cond ce tgmi fgmi =>
    let fg ←  match fgmi with
              | none => pure none
              | some (x) =>  pure (some (← widthGenModItem ctx x))
    pure (.cond ce (← widthGenModItem ctx tgmi) fg)
  | .block gmi => pure (.block (← widthGenModItemList ctx gmi))
  | .module mgi => pure (.module (← widthModOrGen ctx mgi))

def widthGenModItemList (ctx: ModuleCtx)  (gmi:List generate_module_item): trsOk (List generate_module_item) := do
 match gmi with
  | [] => pure []
  | elem:: res => pure ((← widthGenModItem ctx elem) :: (←  widthGenModItemList ctx res))

end
def widthNonPort (ctx: ModuleCtx) (np: non_port_module_item): trsOk non_port_module_item := do
  match np with
  | .generated_module_ins (.generated gmi) => pure (.generated_module_ins (.generated (← widthGenModItem ctx gmi)))
  | .module_or_generate_item mog => pure (.module_or_generate_item (← widthModOrGen ctx mog))

def widthModuleItem (ctx: ModuleCtx)  (item:module_item) : trsOk module_item := do
  match item with
  | .port_decl _pd => pure item -- no expression in this type path
  | .non_port np => pure (.non_port (← widthNonPort ctx  np))

def widthModuleItems (ctx: ModuleCtx)  (items:module_items) : trsOk module_items := do
  match items with
  | .one itm => pure (.one (← widthModuleItem ctx itm))
  | .cons itm itms => pure (.cons (← widthModuleItem ctx itm) (← widthModuleItems ctx itms))

def elaborateWidths (ctx: ModuleCtx) (mod: module_decl): trsOk module_decl := do
  match mod with
  | .ansi name params ports items =>  pure (.ansi name params ports (← widthModuleItems ctx items))


-- ## nfupds / pnfupds — state update helpers

-- Update (nw, flops) triple using result of an assignment.
def nfupds (nfr1 nfr2 : NW × Flops) : NW × Flops :=
  (nfr1.1.merge nfr2.1, nfr1.2.merge nfr2.2)

-- Predicate-filtered update of (nw, flops).
def pnfupds (p : VId → Bool) (nfr1 nfr2 : NW × Flops) : NW × Flops :=
  (nfr1.1.mergeWhere p nfr2.1, nfr1.2.mergeWhere p nfr2.2)

private def setStateValue (ctx : ModuleCtx) (ifw : IFW) (nw target : State)
    (path : HPath) (value : Value) : State :=
  let current := (ctx.decls.merge ifw).merge nw
  target.setUsing current path value

-- ## Assignment processing

def trsVAssign (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (nw : NW) : assign → trsOk NW
  | .net lv e => do
      let p ← lvposfind ctx cpos ifw nw lv
      let v ← evalExpr ctx cpos ifw nw e
      let dv ← declValue ctx.decls p
      let dsz ← expectBits dv
      let vsz ← expectBits v
      let cv := HMap.bits (SZ.castD dsz vsz)
      pure (setStateValue ctx ifw nw nw p cv)

def trsVAssigns (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (nw : NW) : assigns → trsOk NW
  | .one a => trsVAssign ctx cpos ifw nw a
  | .cons a as_ => do
      let nw' ← trsVAssign ctx cpos ifw nw a
      trsVAssigns ctx cpos ifw nw' as_

def trsVContAssign (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (nw : NW) : cont_assign → trsOk NW
  | .net nas => trsVAssigns ctx cpos ifw nw nas

-- ## For-loop step helper (non-partial: only calls evalExpr/lvposfind)

def trsVForStep (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (nw : NW) : for_step → trsOk NW
  | .op_assign (.op lv aop e) => do
      let p ← lvposfind ctx cpos ifw nw lv
      let ev ← evalExpr ctx cpos ifw nw e
      let dv ← declValue ctx.decls p
      let dsz ← expectBits dv
      let esz ← expectBits ev
      match assignOpToBinOp aop with
      | none =>
          let cv := HMap.bits (SZ.castD dsz esz)
          pure (setStateValue ctx ifw nw nw p cv)
      | some bop => do
          let lval ← match findState2 p nw ifw with
            | some v => pure v
            | none => .error .undriven
          let lsz ← expectBits lval
          let result := binOpFunc bop lsz esz
          let cv := HMap.bits (SZ.castD dsz result)
          pure (setStateValue ctx ifw nw nw p cv)
  | .inc_or_dec (.inc vid) => do
      let p ← declfind ctx.decls vid
      let v ← wfind ctx ifw nw vid
      let dv ← declValue ctx.decls p
      let vsz ← expectBits v
      let dsz ← expectBits dv
      let result := SZ.bAdd vsz (SZ.mk' 1 vsz.width false)
      let cv := HMap.bits (SZ.castD dsz result)
      pure (setStateValue ctx ifw nw nw p cv)
  | .inc_or_dec (.dec vid) => do
      let p ← declfind ctx.decls vid
      let v ← wfind ctx ifw nw vid
      let dv ← declValue ctx.decls p
      let vsz ← expectBits v
      let dsz ← expectBits dv
      let result := SZ.bSub vsz (SZ.mk' 1 vsz.width false)
      let cv := HMap.bits (SZ.castD dsz result)
      pure (setStateValue ctx ifw nw nw p cv)




private def enterBlock (ctx : ModuleCtx) (cpos : HPath) (ifw : IFW) (nw : NW)
    (declarations : List procedural_decl) : trsOk (ModuleCtx × IFW × NW) := do
  let names := declarations.map (·.name)
  if names.eraseDups.length != names.length then .error .notSupported else pure ()
  let shapes ← sfListMap (fun declaration => do
    pure (declaration.name, ← localShape ctx declaration)) declarations
  let hidden := ctx.hidden.merge ((ifw.merge nw).filter (names.filter (!ctx.localNames.contains ·)))
  let localCtx : ModuleCtx := { ctx with
    decls := ctx.decls.merge ⟨shapes⟩
    localNames := (ctx.localNames ++ names).eraseDups
    hidden := hidden }
  let ifw := ifw.without names
  let nw := nw.without names
  let initialized ← iterate (fun declaration working => do
    match declaration.initializer with
    | some initializer =>
        let shape ← expectBits (← liftOption (localCtx.decls.get? declaration.name) .undeclared)
        let value ← expectBits (← evalExpr localCtx cpos ifw working initializer)
        pure (working.set [.vid declaration.name] (.bits (SZ.castD shape value)))
    | none =>
        match declaration.dtype with
        | .int_vec .bit _ =>
            -- Two-state bit storage has the language-defined zero initial value.
            let shape ← liftOption (localCtx.decls.get? declaration.name) .undeclared
            pure (working.set [.vid declaration.name] shape)
        | _ => pure working) declarations nw
  pure (localCtx, ifw, initialized)

private def assignmentRootName : expression → Option VId
  | .ident name => some name
  | .select target _ | .hierarchical_ident target _ => assignmentRootName target
  | _ => none

-- ## Statement execution (mutual recursion)

mutual

/- Main statement interpreter.
   Returns (new wire values, flop updates, return value). -/
def trsVStatementItem (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (isComb : Bool) : statement_item → NW →
      trsOk (NW × Flops × Option Value)
  | .blocking_assign_normal lv e, nw => do
      let p ← lvposfind ctx cpos ifw nw lv
      let v ← evalExpr ctx cpos ifw nw e
      let dv ← declValue ctx.decls p
      let dsz ← expectBits dv
      let vsz ← expectBits v
      let cv := HMap.bits (SZ.castD dsz vsz)
      pure (setStateValue ctx ifw nw nw p cv, State.empty, none)
  | .nonblocking_assign lv e, nw => do
      if (assignmentRootName lv).any ctx.localNames.contains then
        .error .notSupported else pure ()
      let p ← lvposfind ctx cpos ifw nw lv
      let v ← evalExpr ctx cpos ifw nw e
      let dv ← declValue ctx.decls p
      let dsz ← expectBits dv
      let vsz ← expectBits v
      let cv := HMap.bits (SZ.castD dsz vsz)
      if isComb
        then pure (setStateValue ctx ifw nw nw p cv, State.empty, none)
        else pure (nw, setStateValue ctx ifw nw State.empty p cv, none)
  | .case _ ce css, nw => do
      let cv ← evalExpr ctx cpos ifw nw ce
      trsVStatementCaseV ctx cpos ifw isComb cv css none nw
  | .cond cp ts fs, nw => do
      let cv ← evalExpr ctx cpos ifw nw cp
      let csz ← expectBits cv
      if csz.isZero then
        match fs with
        | none => pure (nw, State.empty, none)
        | some none => pure (nw, State.empty, none)
        | some (some fsi) => trsVStatementItem ctx cpos ifw isComb fsi nw
      else
        match ts with
        | none => pure (nw, State.empty, none)
        | some tsi => trsVStatementItem ctx cpos ifw isComb tsi nw
  | .forever _, _ => .error .notSupported
  | .repeat _ _, _ => .error .notSupported
  | .while _ _, _ => .error .notSupported
  | .do_while _ _, _ => .error .notSupported
  | .for (.var_assigns fias) ce step body, nw => do
      let nw' ← trsVAssigns ctx cpos ifw nw fias
      trsVStatementForLoop ctx cpos ifw isComb
        ce step body 32 nw' State.empty none
  | .return re, nw => do
      let rv ← evalExpr ctx cpos ifw nw re
      pure (nw, State.empty, some rv)
  | .proc_timing_control _ si, nw =>
      trsVStatementItem ctx cpos ifw isComb si nw
  | .local_decl _, _ => .error .notSupported
  | .seq_block stis, nw => do
      let (declarations, body) := blockDecls stis
      if body.any (fun statement => match statement with | .local_decl _ => true | _ => false)
        then .error .notSupported else pure ()
      let (localCtx, ifw', working) ← enterBlock ctx cpos ifw nw declarations
      let (working, pending, result) ←
        trsVStatementSeqBlock localCtx cpos ifw' isComb stis working State.empty none
      let names := declarations.map (·.name)
      pure ((working.without names).merge (nw.filter names), pending.without names, result)
  | .skip, nw => pure (nw, State.empty, none)

termination_by statement _ => sizeOf statement * 33
decreasing_by all_goals (simp_wf <;> omega)

-- Execute the first matching case item, falling back to default after the scan.
def trsVStatementCaseV (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (isComb : Bool)
    (cv : Value) : List (case_item statement_item) → Option statement_item → NW →
      trsOk (NW × Flops × Option Value)
  | [], none, nw => pure (nw, State.empty, none)
  | [], some st, nw => trsVStatementItem ctx cpos ifw isComb st nw
  | (.default st) :: rest, none, nw =>
      trsVStatementCaseV ctx cpos ifw isComb cv rest (some st) nw
  | (.default _) :: rest, some fallback, nw =>
      trsVStatementCaseV ctx cpos ifw isComb cv rest (some fallback) nw
  | (.case ce st) :: rest, fallback, nw => do
      let cev ← evalExpr ctx cpos ifw nw ce
      let csz ← expectBits cv
      let cesz ← expectBits cev
      if SZ.equiv csz cesz
        then trsVStatementItem ctx cpos ifw isComb st nw
        else trsVStatementCaseV ctx cpos ifw isComb cv rest fallback nw

termination_by cases fallback _ => (sizeOf cases + fallback.elim 0 sizeOf) * 33
decreasing_by all_goals (simp_wf <;> omega)

-- Evaluate a for-loop with bounded unrolling (max 2^5 = 32 iterations).
def trsVStatementForLoop (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (isComb : Bool)
    (ce : expression) (step : for_step) (body : statement_item)
    (fuel : Nat) (nw : NW) (flops : Flops) (retv : Option Value) :
      trsOk (NW × Flops × Option Value) := do
  let cv ← evalExpr ctx cpos ifw nw ce
  let csz ← expectBits cv
  if csz.isZero then
    pure (nw, flops, retv)
  else
    match fuel with
    | 0 => .error .notUnfoldable
    | fuel' + 1 => do
        let (nw', fl', rv') ← trsVStatementItem ctx cpos ifw isComb body nw
        let nw'' := nw.merge nw'
        let flops' := flops.merge fl'
        match rv' with
        | none => do
          let nw''' ← trsVForStep ctx cpos ifw nw'' step
          trsVStatementForLoop ctx cpos ifw isComb ce step body
            fuel' nw''' flops' retv
        | some _ =>
          pure (nw'', flops', rv')

termination_by sizeOf body * 33 + fuel
decreasing_by all_goals (simp_wf <;> omega)

-- Execute a sequence of statements.
def trsVStatementSeqBlock (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (isComb : Bool) : List statement_item → NW → Flops →
      Option Value → trsOk (NW × Flops × Option Value)
  | [], nw', fl', rv' => pure (nw', fl', rv')
  | .local_decl _ :: rest, nw', fl', rv' =>
      trsVStatementSeqBlock ctx cpos ifw isComb rest nw' fl' rv'
  | si :: rest, nw', fl', rv' => do
      let (nw'', fl'', rv'') ← trsVStatementItem ctx cpos ifw isComb si nw'
      let nwAcc := nw'.merge nw''
      let flAcc := fl'.merge fl''
      match rv'' with
      | none =>
        trsVStatementSeqBlock ctx cpos ifw isComb rest nwAcc flAcc rv'
      | some _ =>
        pure (nwAcc, flAcc, rv'')

termination_by statements _ _ _ => sizeOf statements * 33
decreasing_by all_goals (simp_wf <;> omega)

end

-- ## Declaration assignment processing

def trsVNetDeclAssign (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (nw : NW) : net_decl_assign → trsOk NW
  | .net vid (some e) => do
      let p ← declfind ctx.decls vid
      let v ← evalExpr ctx cpos ifw nw e
      let dv ← declValue ctx.decls p
      let dsz ← expectBits dv
      let vsz ← expectBits v
      let cv := HMap.bits (SZ.castD dsz vsz)
      pure (setStateValue ctx ifw nw nw p cv)
  | .net _ none => pure nw

def trsVNetDeclAssigns (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (nw : NW) : net_decl_assigns → trsOk NW
  | .one nda => trsVNetDeclAssign ctx cpos ifw nw nda
  | .cons nda ndas => do
      let nw' ← trsVNetDeclAssign ctx cpos ifw nw nda
      trsVNetDeclAssigns ctx cpos ifw nw' ndas

def trsVVarDeclAssign (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (nw : NW) : var_decl_assign → trsOk NW
  | .var vid _ (some e) => do
      let p ← declfind ctx.decls vid
      let v ← evalExpr ctx cpos ifw nw e
      let dv ← declValue ctx.decls p
      let dsz ← expectBits dv
      let vsz ← expectBits v
      let cv := HMap.bits (SZ.castD dsz vsz)
      pure (setStateValue ctx ifw nw nw p cv)
  | .var _ _ none => pure nw

def trsVVarDeclAssigns (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (nw : NW) : var_decl_assigns → trsOk NW
  | .one vda => trsVVarDeclAssign ctx cpos ifw nw vda
  | .cons vda vdas => do
      let nw' ← trsVVarDeclAssign ctx cpos ifw nw vda
      trsVVarDeclAssigns ctx cpos ifw nw' vdas

-- ## Package / generate item declaration processing

def trsVPkgGenItemDecl (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (nw : NW) : pkg_gen_item_decl → trsOk NW
  | .net (.net _ _ ndas) => trsVNetDeclAssigns ctx cpos ifw nw ndas
  | .data (.var_decl (.var _ vdas)) => trsVVarDeclAssigns ctx cpos ifw nw vdas
  | _ => pure nw

def alwaysIsComb : always_keyword → trsOk Bool
  | .always_comb => pure true
  | .always_ff => pure false
  | .always_latch => .error .notSupported
  | .always => .error .notSupported

mutual
def validateAlwaysStatementItem (allowBlocking allowNonblocking : Bool) :
    statement_item → trsOk Unit
  | .blocking_assign_normal _ _ =>
      if allowBlocking then pure () else .error .notSupported
  | .nonblocking_assign _ _ =>
      if allowNonblocking then pure () else .error .notSupported
  | .skip => pure ()
  | .local_decl _ => .error .notSupported
  | .case _ _ items =>
      validateAlwaysCaseItems allowBlocking allowNonblocking items
  | .cond _ thenStmt elseStmt => do
      match thenStmt with
      | none => pure ()
      | some statement =>
          validateAlwaysStatementItem allowBlocking allowNonblocking statement
      match elseStmt with
      | none | some none => pure ()
      | some (some statement) =>
          validateAlwaysStatementItem allowBlocking allowNonblocking statement
  | .forever body | .repeat _ body | .while _ body | .for _ _ _ body |
      .do_while body _ =>
      validateAlwaysStatementItem allowBlocking allowNonblocking body
  | .return _ => .error .notSupported
  | .proc_timing_control _ _ => .error .notSupported
  | .seq_block statements =>
      validateAlwaysStatementItems allowBlocking allowNonblocking true statements

def validateAlwaysStatementItems (allowBlocking allowNonblocking declarationsAllowed : Bool) :
    List statement_item → trsOk Unit
  | [] => pure ()
  | .local_decl _ :: rest => do
      if declarationsAllowed then pure ()
      else .error .notSupported
      validateAlwaysStatementItems allowBlocking allowNonblocking true rest
  | statement :: rest => do
      validateAlwaysStatementItem allowBlocking allowNonblocking statement
      validateAlwaysStatementItems allowBlocking allowNonblocking false rest

def validateAlwaysCaseItems (allowBlocking allowNonblocking : Bool) :
    List (case_item statement_item) → trsOk Unit
  | [] => pure ()
  | item :: rest => do
      match item with
      | .default statement | .case _ statement =>
          validateAlwaysStatementItem allowBlocking allowNonblocking statement
      validateAlwaysCaseItems allowBlocking allowNonblocking rest
end

def eventExpressionHasOnlyEdges : event_expression → Bool
  | .expr (some _) _ => true
  | .expr none _ => false
  | .or left right =>
      eventExpressionHasOnlyEdges left && eventExpressionHasOnlyEdges right

-- Plain Verilog always blocks are supported for one rising-edge clock.
-- A transition call represents that edge; it is not a waveform scheduler.
def eventExpressionIsSinglePosedgeClock : event_expression → Bool
  | .expr (some .posedge) (.ident _) => true
  | _ => false

def alwaysBlockIsComb (keyword : always_keyword) (body : statement_item) : trsOk Bool :=
  match keyword, body with
  | .always, .proc_timing_control (.event (.expr event)) _ =>
      if eventExpressionIsSinglePosedgeClock event then pure false
      else .error .notSupported
  | .always, _ => .error .notSupported
  | _, _ => alwaysIsComb keyword

def validateAlwaysBlock : always_keyword → statement_item → trsOk Unit
  | .always_comb, body => validateAlwaysStatementItem true false body
  | .always_ff, .proc_timing_control (.event (.expr event)) body =>
      if eventExpressionHasOnlyEdges event then
        validateAlwaysStatementItem false true body
      else
        .error .notSupported
  | .always, .proc_timing_control (.event (.expr event)) body =>
      if eventExpressionIsSinglePosedgeClock event then
        validateAlwaysStatementItem false true body
      else
        .error .notSupported
  | _, _ => .error .notSupported

def trsVModuleCommonItem (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (_isComb : Bool) : module_common_item → NW → trsOk (NW × Flops)
  | .decl (.pkg pgid), nw => do
      let nw' ← trsVPkgGenItemDecl ctx cpos ifw nw pgid
      pure (nw', State.empty)
  | .cont_assign ca, nw => do
      let nw' ← trsVContAssign ctx cpos ifw nw ca
      pure (nw', State.empty)
  | .always ak (.stmt si), nw => do
      let alwaysComb ← alwaysBlockIsComb ak si
      validateAlwaysBlock ak si
      let (nw', fl, _) ← trsVStatementItem ctx cpos ifw alwaysComb si nw
      pure (nw', fl)
  | .initial _, _ => .error .notSupported
  | .assert _, _ => .error .notSupported

-- ## Module instantiation

def namedPortConnsList : named_port_conns → List named_port_conn
  | .one npc => [npc]
  | .cons npc npcs => npc :: namedPortConnsList npcs

private def namedPortConnId? : named_port_conn → Option VId
  | .ident pid => some pid
  | .open pid => some pid
  | .expr pid _ => some pid
  | .wildcard => none

def namedPortConnFor (npcs : named_port_conns) (pid : VId) : Option named_port_conn :=
  let conns := namedPortConnsList npcs
  match conns.find? (fun conn => namedPortConnId? conn == some pid) with
  | some conn => some conn
  | none => if conns.any (fun conn => conn == .wildcard) then some (.ident pid) else none

private def hasDuplicatePortIds : List VId → Bool
  | [] => false
  | pid :: rest => rest.contains pid || hasDuplicatePortIds rest

private def hasMultipleWildcards : List named_port_conn → Bool
  | [] => false
  | .wildcard :: rest => rest.any (fun conn => conn == .wildcard)
  | _ :: rest => hasMultipleWildcards rest

private def validateNamedPortConns (mtrs : MTrs) (npcs : named_port_conns) :
    trsOk Unit :=
  let conns := namedPortConnsList npcs
  let portIds := conns.filterMap namedPortConnId?
  let knownPortIds := mtrs.inputVids ++ mtrs.outputVids
  if portIds.any (fun pid => !knownPortIds.contains pid) then .error .undeclared
  else if hasDuplicatePortIds portIds || hasMultipleWildcards conns then
    .error .notSupported
  else
    pure ()

private def castModuleInput (mtrs : MTrs) (pid : VId) (value : Value) :
    trsOk Value := do
  let inputDecl ← liftOption (mtrs.inputDecls.get? pid) .undeclared
  let inputBits ← expectBits inputDecl
  let valueBits ← expectBits value
  pure (HMap.bits (SZ.castD inputBits valueBits))

private def trsVModuleInputArg (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (nw : NW) (mtrs : MTrs) (npcs : named_port_conns) (pid : VId) :
      trsOk (Option Value) := do
  let value? ← match namedPortConnFor npcs pid with
    | some (.ident parentVid) => do
        let value ← wfind ctx ifw nw parentVid
        pure (some value)
    | some (.expr _ e) => do -- resizing e as an assignment
        let inputDecl ← liftOption (mtrs.inputDecls.get? pid) .undeclared
        let newex ← widthAssignLike ctx (valWidth inputDecl) e
        let value ← evalExpr ctx cpos ifw nw newex
        pure (some value)
    | some (.open _) | none => pure none
    | some .wildcard => .error .fatal
  match value? with
  | none => pure none
  | some value => do
      let value' ← castModuleInput mtrs pid value
      pure (some value')

def trsVModuleInsMTrsArgs (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (nw : NW) (mtrs : MTrs) : named_port_conns →
      trsOk (List (Option Value))
  | npcs => sfListMap (trsVModuleInputArg ctx cpos ifw nw mtrs npcs) mtrs.inputVids

private def trsVModuleOutput (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (newWires : NW) (npcs : named_port_conns)
    (nw : NW) (ovid : VId) : trsOk NW := do
  let target ← match namedPortConnFor npcs ovid with
    | some (.ident parentVid) => pure (some (.ident parentVid))
    | some (.expr _ e) => pure (some e)
    | some (.open _) | none => pure none
    | some .wildcard => .error .fatal
  match target with
  | none => pure nw
  | some lv => do
      let ov ← liftOption (newWires.get? ovid) .undriven
      let lv_resz ← widthLvalueAssignLike ctx lv --sizing the lhs
      let p ← lvposfind ctx cpos ifw nw lv_resz
      let dv ← declValue ctx.decls p
      let dsz ← expectBits dv
      let osz ← expectBits ov
      pure (setStateValue ctx ifw nw nw p (HMap.bits (SZ.castD dsz osz)))

private def trsVModuleOutputs (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (newWires : NW) (npcs : named_port_conns) :
    List VId → NW → trsOk NW
  | [], nw => pure nw
  | ovid :: rest, nw => do
      let nw' ← trsVModuleOutput ctx cpos ifw newWires npcs nw ovid
      trsVModuleOutputs ctx cpos ifw newWires npcs rest nw'

def trsVModuleInsMTrs (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (ifw : IFW) (flops : Flops) : module_ins → NW → trsOk (NW × Flops)
  | .module mid _ (.hier iid (.named npcs)), nw => do
      let mtrs ← mtrss mid
      validateNamedPortConns mtrs npcs
      let args ← trsVModuleInsMTrsArgs ctx cpos ifw nw mtrs npcs
      let inputState := buildOptionalInputState mtrs.inputVids args
      let flopState := (flops.child? iid).getD State.empty
      let (newWires, newFlops) := mtrs.func inputState flopState
      let nw' ← trsVModuleOutputs ctx cpos ifw newWires npcs mtrs.outputVids nw
      pure (nw', State.nested iid newFlops)

def trsVModuleIns (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (ifw : IFW) (flops : Flops) : module_ins → NW → trsOk (NW × Flops)
  | mi, nw => trsVModuleInsMTrs ctx mtrss cpos ifw flops mi nw

def trsVModuleOrGenerateItem (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (ifw : IFW) (flops : Flops) (isComb : Bool) : module_or_generate_item → NW → trsOk (NW × Flops)
  | .common ci, nw => trsVModuleCommonItem ctx cpos ifw isComb ci nw
  | .ins mi, nw => trsVModuleIns ctx mtrss cpos ifw flops mi nw

-- ## iffupds — update IFW and flops

def iffupds (iff1 iff2 : IFF) : IFF :=
  (iff1.1.merge iff2.1, iff1.2.merge iff2.2)

-- ## Generate module items

mutual
def trsVGenerateModuleItem (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (ifw : IFW) (flops : Flops) (isComb : Bool) : generate_module_item → NW → trsOk (NW × Flops)
  | .module mogi, nw =>
      match trsVModuleOrGenerateItem ctx mtrss cpos ifw flops isComb mogi nw with
      | .ok result => .ok result
      | .error .undriven =>
          if ctx.deferUndriven then pure (nw, State.empty) else .error .undriven
      | .error failure => .error failure
  | .cond ce tgmi fgmi, nw => do
      let cv ← evalConstW ce  ctx.consts 0
      let csz ← expectBits cv
      if csz.isZero then
        match fgmi with
        | none => pure (nw, State.empty)
        | some fgmi' => trsVGenerateModuleItem ctx mtrss cpos ifw flops isComb fgmi' nw
      else
        trsVGenerateModuleItem ctx mtrss cpos ifw flops isComb tgmi nw
  | .block gmis, nw =>
      trsVGenerateModuleItemList ctx mtrss cpos ifw flops isComb gmis nw

def trsVGenerateModuleItemList (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (ifw : IFW) (flops : Flops) (isComb : Bool) : List generate_module_item → NW → trsOk (NW × Flops)
  | [], _ => .ok (State.empty, State.empty)
  | gmi :: rest, nw => do
      let b ← trsVGenerateModuleItem ctx mtrss cpos ifw flops isComb gmi nw
      let nw' := nw.merge b.1
      let brest ← trsVGenerateModuleItemList ctx mtrss cpos ifw flops isComb rest nw'
      pure (nfupds b brest)
end

-- ## Non-port module items / module items

def trsVNonPortModuleItem (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (ifw : IFW) (flops : Flops) (isComb : Bool) : non_port_module_item → NW → trsOk (NW × Flops)
  | .generated_module_ins (.generated gmi), nw =>
      trsVGenerateModuleItem ctx mtrss cpos ifw flops isComb gmi nw
  | .module_or_generate_item mogi, nw =>
      match trsVModuleOrGenerateItem ctx mtrss cpos ifw flops isComb mogi nw with
      | .ok result => .ok result
      | .error .undriven =>
          if ctx.deferUndriven then pure (nw, State.empty) else .error .undriven
      | .error failure => .error failure

def trsVModuleItem (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (ifw : IFW) (flops : Flops) (isComb : Bool) : module_item → NW → trsOk (NW × Flops)
  | .port_decl _, nw => pure (nw, State.empty)
  | .non_port np, nw => trsVNonPortModuleItem ctx mtrss cpos ifw flops isComb np nw

def trsVModuleItems (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (ifw : IFW) (flops : Flops) (isComb : Bool) : module_items → NW → trsOk (NW × Flops)
  | .one mi, nw => trsVModuleItem ctx mtrss cpos ifw flops isComb mi nw
  | .cons mi mis, nw => do
      let (nw', fl') ← trsVModuleItem ctx mtrss cpos ifw flops isComb mi nw
      let (nw'', fl'') ← trsVModuleItems ctx mtrss cpos ifw flops isComb mis (nw.merge nw')
      pure (nw'.merge nw'', fl'.merge fl'')

-- ## Module declaration — building the transition function

def trsVModuleDecl (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (m : module_decl) : State → State → trsOk (NW × Flops) :=
  match m with
  | .ansi _ _ _ mitems => fun ifw flops =>
      trsVModuleItems ctx mtrss cpos ifw flops true mitems State.empty

-- Build the IFF (combined IFW × Flops) transition.
def trsVModuleDecl_IFF (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (m : module_decl) : IFF → trsOk IFF :=
  fun (ifw, flops) => do
    let (nw, fl) ← trsVModuleDecl ctx mtrss cpos m ifw flops
    pure (ifw.merge nw, flops.merge fl)

-- ## Fixed-point iteration

-- Iterate until the transition is stable, bounded by `fuel` passes.
def trsM_iff_fix (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (m : module_decl) : Nat → IFF → trsOk IFF
  | 0, _ => .error .notUnfoldable
  | fuel + 1, iff_ => do
      let iff' ← trsVModuleDecl_IFF ctx mtrss cpos m iff_
      if iff'.1 == iff_.1 then
        trsVModuleDecl_IFF { ctx with deferUndriven := false } mtrss cpos m iff_
      else trsM_iff_fix ctx mtrss cpos m fuel (iff'.1, iff_.2)

-- Build the final MTrs for a module, requiring convergence within five passes.
def trsM_IFF (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (m : module_decl) : IFF → trsOk IFF :=
  trsM_iff_fix ctx mtrss cpos m 5

-- ## trsNext / trsT — compute next state and extract output

def trsNext (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (m : module_decl)
    (inputs : State) (flops : Flops) : trsOk (State × Flops) := do
  let ifw := flops.merge inputs
  let m_widths ← match ctx.widthElaboratedModule with
                  | some n => pure n
                  | none => elaborateWidths ctx m
  let (ifw', flops') ← trsM_IFF ctx mtrss cpos m_widths (ifw, flops) -- use new module
  pure (ifw', flops')

def trsT (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (m : module_decl)
    (outputVids : List VId) (inputs : State) (flops : Flops) : trsOk (State × Flops) := do
  let (ifw', flops') ← trsNext ctx mtrss cpos m inputs flops
  pure (ifw'.filter outputVids, flops')

-- ## Declaration collection

def declsVNetDeclAssigns (consts : Consts) (pd : packed_dims) :
    net_decl_assigns → trsOk (List (VId × Value))
  | .one (.net vid _) => do
      let dv ← declPortType consts (.port (some .wire) pd)
      pure [(vid, dv)]
  | .cons (.net vid _) ndas => do
      let dv ← declPortType consts (.port (some .wire) pd)
      let rest ← declsVNetDeclAssigns consts pd ndas
      pure ((vid, dv) :: rest)

def declsVVarDeclAssign (tdefs : TDefs) (consts : Consts) (dt : data_type) :
    var_decl_assign → trsOk (VId × Value)
  | .var vid vd _ => do
      let basev ← declDataType tdefs consts dt
      let dv ← wrapUnpacked consts basev vd
      pure (vid, dv)

def declsVVarDeclAssigns (tdefs : TDefs) (consts : Consts) (dt : data_type) :
    var_decl_assigns → trsOk (List (VId × Value))
  | .one vda => do let r ← declsVVarDeclAssign tdefs consts dt vda; pure [r]
  | .cons vda vdas => do
      let r ← declsVVarDeclAssign tdefs consts dt vda
      let rest ← declsVVarDeclAssigns tdefs consts dt vdas
      pure (r :: rest)

def declsVParamAssign (tdefs : TDefs) (consts : Consts) (dti : data_type_or_implicit) :
    param_assign → trsOk (VId × Value)
  | .param pid _ => do
      let dv ← declDataTypeOrImplicit tdefs consts dti
      pure (pid, dv)

def declsVParamAssigns (tdefs : TDefs) (consts : Consts) (dti : data_type_or_implicit) :
    param_assigns → trsOk (List (VId × Value))
  | .one pa => do let r ← declsVParamAssign tdefs consts dti pa; pure [r]
  | .cons pa pas => do
      let r ← declsVParamAssign tdefs consts dti pa
      let rest ← declsVParamAssigns tdefs consts dti pas
      pure (r :: rest)

def declsVPkgGenItemDecl (tdefs : TDefs) (consts : Consts) : pkg_gen_item_decl → trsOk (List (VId × Value))
  | .net (.net _ pd ndas) => declsVNetDeclAssigns consts pd ndas
  | .data (.var_decl (.var dt vdas)) => declsVVarDeclAssigns tdefs consts dt vdas
  | .param (.data dti pas) => declsVParamAssigns tdefs consts dti pas
  | .local_param (.local dti pas) => declsVParamAssigns tdefs consts dti pas
  | _ => pure []

def declsVModuleCommonItem (tdefs : TDefs) (consts : Consts) : module_common_item → trsOk (List (VId × Value))
  | .decl (.pkg pgid) => declsVPkgGenItemDecl tdefs consts pgid
  | _ => pure []

def declsVModuleOrGenerateItem (tdefs : TDefs) (consts : Consts) : module_or_generate_item → trsOk (List (VId × Value))
  | .common ci => declsVModuleCommonItem tdefs consts ci
  | .ins _ => pure []

mutual
def declsVGenerateModuleItem (tdefs : TDefs) (consts : Consts) : generate_module_item → trsOk (List (VId × Value))
  | .module mogi => declsVModuleOrGenerateItem tdefs consts mogi
  | .cond _ tgmi fgmi => do
      let td ← declsVGenerateModuleItem tdefs consts tgmi
      let fd ← match fgmi with
        | none => pure []
        | some fgmi' => declsVGenerateModuleItem tdefs consts fgmi'
      pure (td ++ fd)
  | .block gmis => declsVGenerateModuleItemList tdefs consts gmis

def declsVGenerateModuleItemList (tdefs : TDefs) (consts : Consts) : List generate_module_item → trsOk (List (VId × Value))
  | [] => pure []
  | gmi :: rest => do
      let d ← declsVGenerateModuleItem tdefs consts gmi
      let rest' ← declsVGenerateModuleItemList tdefs consts rest
      pure (d ++ rest')
end

def declsVNonPortModuleItem (tdefs : TDefs) (consts : Consts) : non_port_module_item → trsOk (List (VId × Value))
  | .generated_module_ins (.generated gmi) => declsVGenerateModuleItem tdefs consts gmi
  | .module_or_generate_item mogi => declsVModuleOrGenerateItem tdefs consts mogi

def declsVModuleItem (tdefs : TDefs) (consts : Consts) : module_item → trsOk (List (VId × Value))
  | .port_decl _ => pure []
  | .non_port np => declsVNonPortModuleItem tdefs consts np

def declsVModuleItems (tdefs : TDefs) (consts : Consts) : module_items → trsOk (List (VId × Value))
  | .one mi => declsVModuleItem tdefs consts mi
  | .cons mi mis => do
      let d ← declsVModuleItem tdefs consts mi
      let rest ← declsVModuleItems tdefs consts mis
      pure (d ++ rest)

def declsVParamDecl (tdefs : TDefs) (consts : Consts) : param_decl → trsOk (List (VId × Value))
  | .data dti pas => declsVParamAssigns tdefs consts dti pas

def declsVParamPorts (tdefs : TDefs) (consts : Consts) : param_ports → trsOk (List (VId × Value))
  | .nil => pure []
  | .one pd => declsVParamDecl tdefs consts pd
  | .cons pd pds => do
      let d ← declsVParamDecl tdefs consts pd
      let rest ← declsVParamPorts tdefs consts pds
      pure (d ++ rest)


def declsVModuleDecl (tdefs : TDefs) (consts : Consts) : module_decl → trsOk Decls
  | .ansi _ pps pdecls mitems => do
      let ppd ← declsVParamPorts tdefs consts pps
      let apd ← declsVAnsiPortDecls tdefs consts pdecls
      let mid ← declsVModuleItems tdefs consts mitems
      pure ⟨ppd ++ apd ++ mid⟩

-- ## Function collection

private def functionPortIsInput : ansi_port_decl → Bool
  | .net none _ | .var none _ _ => true
  | .net (some (.net direction _)) _ =>
      direction == none || direction == some .input
  | .var (some (.var direction _)) _ _ =>
      direction == none || direction == some .input

private def functionPortsAreInputs : ansi_port_decls → Bool
  | .nil => true
  | .one port => functionPortIsInput port
  | .cons port rest => functionPortIsInput port && functionPortsAreInputs rest

private def castFunctionInputs (portDecls : Decls) :
    List VId → State → trsOk State
  | [], _ => pure State.empty
  | pid :: rest, inputs => do
      let inputDecl ← liftOption (portDecls.get? pid) .undeclared
      let inputValue ← liftOption (inputs.get? pid) .undriven
      let inputBits ← expectBits inputDecl
      let valueBits ← expectBits inputValue
      let rest' ← castFunctionInputs portDecls rest inputs
      pure (rest'.set [.vid pid] (HMap.bits (SZ.castD inputBits valueBits)))

def funcsVFuncDecl (ctx : ModuleCtx) (cpos : HPath) :
    func_decl → trsOk (VId × Func)
  | .func dti fid ports (.stmt si) => do
      if functionPortsAreInputs ports then pure () else .error .notSupported
      let inputVids := funcsPortVids ports
      let portDecls ← declsVAnsiPortDecls ctx.tdefs ctx.consts ports
      let returnDecl ← declDataTypeOrImplicit ctx.tdefs ctx.consts dti
      let f : Func := {
        returnShape := returnDecl
        inputShapes := portDecls.map (·.2)
        inputVids := inputVids
        func := fun callStack callerIfw callerNw inputState => do
          let inputState' ← castFunctionInputs ⟨portDecls⟩ inputVids inputState
          let funcCtx :=
            { ctx with
              decls := ctx.decls.merge ⟨portDecls⟩
              funcs := guardFunctionCalls ctx.funcs callStack
              localNames := inputVids
              hidden := (callerIfw.merge callerNw).filter inputVids }
          let ifw := (callerIfw.merge callerNw).merge inputState'
          let (_, _, rv) ← trsVStatementItem funcCtx cpos ifw true si State.empty
          let returnValue ← liftOption rv .undriven
          let returnBits ← expectBits returnDecl
          let valueBits ← expectBits returnValue
          pure (HMap.bits (SZ.castD returnBits valueBits))
      }
      pure (fid, f)
where
  funcsPortVids : ansi_port_decls → List VId
    | .nil => []
    | .one (.net _ pid) => [pid]
    | .one (.var _ pid _) => [pid]
    | .cons (.net _ pid) rest => pid :: funcsPortVids rest
    | .cons (.var _ pid _) rest => pid :: funcsPortVids rest

def funcsVPkgGenItemDecl (ctx : ModuleCtx) (cpos : HPath) :
    pkg_gen_item_decl → trsOk (List (VId × Func))
  | .func fd => do let r ← funcsVFuncDecl ctx cpos fd; pure [r]
  | _ => pure []

def funcsVModuleCommonItem (ctx : ModuleCtx) (cpos : HPath) :
    module_common_item → trsOk (List (VId × Func))
  | .decl (.pkg pgid) => funcsVPkgGenItemDecl ctx cpos pgid
  | _ => pure []

def funcsVModuleOrGenerateItem (ctx : ModuleCtx) (cpos : HPath) :
    module_or_generate_item → trsOk (List (VId × Func))
  | .common ci => funcsVModuleCommonItem ctx cpos ci
  | .ins _ => pure []

mutual
def funcsVGenerateModuleItem (ctx : ModuleCtx) (cpos : HPath) :
    generate_module_item → trsOk (List (VId × Func))
  | .module mogi => funcsVModuleOrGenerateItem ctx cpos mogi
  | .cond _ tgmi fgmi => do
      let td ← funcsVGenerateModuleItem ctx cpos tgmi
      let fd ← match fgmi with
        | none => pure []
        | some fgmi' => funcsVGenerateModuleItem ctx cpos fgmi'
      pure (td ++ fd)
  | .block gmis => funcsVGenerateModuleItemList ctx cpos gmis

def funcsVGenerateModuleItemList (ctx : ModuleCtx) (cpos : HPath) :
    List generate_module_item → trsOk (List (VId × Func))
  | [] => pure []
  | gmi :: rest => do
      let d ← funcsVGenerateModuleItem ctx cpos gmi
      let rest' ← funcsVGenerateModuleItemList ctx cpos rest
      pure (d ++ rest')
end

def funcsVNonPortModuleItem (ctx : ModuleCtx) (cpos : HPath) :
    non_port_module_item → trsOk (List (VId × Func))
  | .generated_module_ins (.generated gmi) => funcsVGenerateModuleItem ctx cpos gmi
  | .module_or_generate_item mogi => funcsVModuleOrGenerateItem ctx cpos mogi

def funcsVModuleItem (ctx : ModuleCtx) (cpos : HPath) :
    module_item → trsOk (List (VId × Func))
  | .port_decl _ => pure []
  | .non_port np => funcsVNonPortModuleItem ctx cpos np

def funcsVModuleItems (ctx : ModuleCtx) (cpos : HPath) :
    module_items → trsOk (List (VId × Func))
  | .one mi => funcsVModuleItem ctx cpos mi
  | .cons mi mis => do
      let d ← funcsVModuleItem ctx cpos mi
      let rest ← funcsVModuleItems ctx cpos mis
      pure (d ++ rest)

partial def funcsVModuleDeclLookup (tdefs : TDefs) (decls : Decls) (consts : Consts)
    (cpos : HPath) (m : module_decl) : Funcs :=
  fun fid =>
    match m with
    | .ansi _ _ _ mitems =>
        let ctx : ModuleCtx :=
          { tdefs, decls,
            funcs := funcsVModuleDeclLookup tdefs decls consts cpos m, consts }
        match funcsVModuleItems ctx cpos mitems with
        | .error err => .error err
        | .ok funcList =>
            let funcs := funcList.foldl
              (fun acc (vid, f) => fmapMerge (fmapSingle vid f) acc) fmapEmpty
            funcs fid

def funcsVModuleDecl (tdefs : TDefs) (decls : Decls) (consts : Consts) (cpos : HPath)
    (m : module_decl) : trsOk Funcs :=
  pure (funcsVModuleDeclLookup tdefs decls consts cpos m)

def moduleCtxVModuleDecl (cpos : HPath) (m : module_decl) : trsOk ModuleCtx := do
  let consts0 ← computeConsts m
  let (tdefs, consts) ← computeTDefs consts0 m
  let decls ← declsVModuleDecl tdefs consts m
  let funcs ← funcsVModuleDecl tdefs decls consts cpos m
  let ctx0 : ModuleCtx := { tdefs, decls, funcs, consts }
  let widthElaboratedModule ← elaborateWidths ctx0 m  -- build a new module with the determined widths
  let funcs_widths ← funcsVModuleDecl tdefs decls consts cpos widthElaboratedModule
  pure { tdefs, decls, funcs:=funcs_widths, consts , widthElaboratedModule}

-- ## Parameter ports — with module-level context

def declsVParamPortsM (m : module_decl) : trsOk (List (VId × Value)) := do
  let consts0 ← computeConsts m
  let (tdefs, consts) ← computeTDefs consts0 m
  match m with
  | .ansi _ pps _ _ => declsVParamPorts tdefs consts pps

end VerilLean.Lang.Semantics
