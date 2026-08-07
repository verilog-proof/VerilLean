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
  deriving Inhabited, Repr, BEq

namespace State

def empty : State := ⟨[]⟩

def toValue (state : State) : Value := .str state.fields

def ofValue? : Value → Option State
  | .str fields => some ⟨fields⟩
  | _ => none

private def findValue? : Value → HPath → Option Value
  | value, [] => some value
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

private def setValue : Value → HPath → Value → Value
  | _, [], value => value
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
  outputVids : List VId
  func       : State → State → (State × State)

structure Func where
  inputVids : List VId
  func      : State → Option Value

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

def trsToOption : trsOk A → Option A
  | .ok value => some value
  | .error _ => none

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
  decls  : Decls
  funcs  : Funcs
  consts : Consts

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

-- ## Declaration size helpers

/- Evaluate packed dimensions to get the declaration size (as HMap).
   Returns `none` for `nil`. -/
def evalPackedDims (consts : Consts) : packed_dims → trsOk (Option Value)
  | .nil => .ok none
  | .one (.range lr rr) => do
      let lv ← evalConst consts lr
      let rv ← evalConst consts rr
      let lsz ← expectBits lv
      let rsz ← expectBits rv
      let w := (lsz.norm - rsz.norm).toNat + 1
      pure (some (HMap.bits (SZ.mk' 0 w false)))
  | .one (.one de) => do
      let dv ← evalConst consts de
      let dsz ← expectBits dv
      let w := dsz.norm.toNat
      pure (some (HMap.bits (SZ.mk' 0 w false)))
  | .cons pd pds => do
      let _ ← match pd with
        | .range lr rr => do
            let lv ← evalConst consts lr
            let rv ← evalConst consts rr
            let lsz ← expectBits lv
            let rsz ← expectBits rv
            let w := (lsz.norm - rsz.norm).toNat + 1
            pure (some (HMap.bits (SZ.mk' 0 w false)))
        | .one de => do
            let dv ← evalConst consts de
            let dsz ← expectBits dv
            let w := dsz.norm.toNat
            pure (some (HMap.bits (SZ.mk' 0 w false)))
      evalPackedDims consts pds

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
  | .one de => do let dv ← evalConst consts de; pure (hbits dv).norm.toNat
  | .range lr rr => do
      let lv ← evalConst consts lr
      let rv ← evalConst consts rr
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
  let v ← evalConst consts ce
  match dti with
  | .implicit .nil => pure v
  | _ => do
      let dv ← declDataTypeOrImplicit HMap.empty consts dti
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
  | .cond _ tgmi fgmi => do
      let consts' ← collectParamsGenerateModuleItem consts tgmi
      match fgmi with
      | none => pure consts'
      | some fgmi' => collectParamsGenerateModuleItem consts' fgmi'
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
        | some ce => do let v ← evalConst consts ce; pure (hbits v).norm
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
          match haccessO ctx.consts vid with
          | some v => pure v
          | none => .error .undriven
  | .error _ =>
      match haccessO ctx.consts vid with
      | some v => pure v
      | none => .error .undeclared

-- ## getAccessVid — extract vid from a "child" expression

def getAccessVid : expression → trsOk VId
  | .ident vid => .ok vid
  | _ => .error .notSupported

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
      let inputState := buildFInputState f.inputVids avs
      liftOption (f.func inputState) .undriven
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

-- ## nfupds / pnfupds — state update helpers

-- Update (nw, flops) triple using result of an assignment.
def nfupds (nfr1 nfr2 : NW × Flops) : NW × Flops :=
  (nfr1.1.merge nfr2.1, nfr1.2.merge nfr2.2)

-- Predicate-filtered update of (nw, flops).
def pnfupds (p : VId → Bool) (nfr1 nfr2 : NW × Flops) : NW × Flops :=
  (nfr1.1.mergeWhere p nfr2.1, nfr1.2.mergeWhere p nfr2.2)

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
      pure (nw.set p cv)

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
          pure (nw.set p cv)
      | some bop => do
          let lval ← match findState2 p nw ifw with
            | some v => pure v
            | none => .error .undriven
          let lsz ← expectBits lval
          let result := binOpFunc bop lsz esz
          let cv := HMap.bits (SZ.castD dsz result)
          pure (nw.set p cv)
  | .inc_or_dec (.inc vid) => do
      let p ← declfind ctx.decls vid
      let v ← wfind ctx ifw nw vid
      let dv ← declValue ctx.decls p
      let vsz ← expectBits v
      let dsz ← expectBits dv
      let result := SZ.bAdd vsz (SZ.mk' 1 vsz.width false)
      let cv := HMap.bits (SZ.castD dsz result)
      pure (nw.set p cv)
  | .inc_or_dec (.dec vid) => do
      let p ← declfind ctx.decls vid
      let v ← wfind ctx ifw nw vid
      let dv ← declValue ctx.decls p
      let vsz ← expectBits v
      let dsz ← expectBits dv
      let result := SZ.bSub vsz (SZ.mk' 1 vsz.width false)
      let cv := HMap.bits (SZ.castD dsz result)
      pure (nw.set p cv)

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
      pure (nw.set p cv, State.empty, none)
  | .nonblocking_assign lv e, nw => do
      let p ← lvposfind ctx cpos ifw nw lv
      let v ← evalExpr ctx cpos ifw nw e
      let dv ← declValue ctx.decls p
      let dsz ← expectBits dv
      let vsz ← expectBits v
      let cv := HMap.bits (SZ.castD dsz vsz)
      if isComb
        then pure (nw.set p cv, State.empty, none)
        else pure (nw, State.singleton p cv, none)
  | .case _ ce css, nw => do
      let cv ← evalExpr ctx cpos ifw nw ce
      trsVStatementCaseV ctx cpos ifw isComb cv css nw
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
  | .forever _, nw => pure (nw, State.empty, none)  -- skip
  | .repeat _ _, nw => pure (nw, State.empty, none)  -- skip
  | .while _ _, nw => pure (nw, State.empty, none)  -- skip
  | .do_while _ _, nw => pure (nw, State.empty, none)  -- skip
  | .for (.var_assigns fias) ce step body, nw => do
      let nw' ← trsVAssigns ctx cpos ifw nw fias
      trsVStatementForLoop ctx cpos ifw isComb
        ce step body 32 nw' State.empty none
  | .return re, nw => do
      let rv ← evalExpr ctx cpos ifw nw re
      pure (nw, State.empty, some rv)
  | .proc_timing_control _ si, nw =>
      trsVStatementItem ctx cpos ifw isComb si nw
  | .seq_block stis, nw =>
      trsVStatementSeqBlock ctx cpos ifw isComb stis nw State.empty none
  | .skip, nw => pure (nw, State.empty, none)

-- Process a case statement: find matching case item and execute.
def trsVStatementCaseV (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (isComb : Bool)
    (cv : Value) : List (case_item statement_item) → NW →
      trsOk (NW × Flops × Option Value)
  | [], nw => pure (nw, State.empty, none)
  | (.default st) :: _, nw => trsVStatementItem ctx cpos ifw isComb st nw
  | (.case ce st) :: rest, nw => do
      let cev ← evalExpr ctx cpos ifw nw ce
      let csz ← expectBits cv
      let cesz ← expectBits cev
      if SZ.equiv csz cesz
        then trsVStatementItem ctx cpos ifw isComb st nw
        else trsVStatementCaseV ctx cpos ifw isComb cv rest nw

-- Evaluate a for-loop with bounded unrolling (max 2^5 = 32 iterations).
def trsVStatementForLoop (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (isComb : Bool)
    (ce : expression) (step : for_step) (body : statement_item)
    (fuel : Nat) (nw : NW) (flops : Flops) (retv : Option Value) :
      trsOk (NW × Flops × Option Value) :=
  match fuel with
  | 0 => pure (nw, flops, retv)
  | fuel' + 1 => do
    let cv ← evalExpr ctx cpos ifw nw ce
    let csz ← expectBits cv
    if csz.isZero
      then pure (nw, flops, retv)
      else do
        let (nw', fl', rv') ← trsVStatementItem ctx cpos ifw isComb body nw
        let nw'' := nw.merge nw'
        let flops' := flops.merge fl'
        let retv' := match rv' with
          | some value => some value
          | none => retv
        -- apply step
        let nw''' ← trsVForStep ctx cpos ifw nw'' step
        trsVStatementForLoop ctx cpos ifw isComb ce step body
          fuel' nw''' flops' retv'

-- Execute a sequence of statements.
def trsVStatementSeqBlock (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (isComb : Bool) : List statement_item → NW → Flops →
      Option Value → trsOk (NW × Flops × Option Value)
  | [], nw', fl', rv' => pure (nw', fl', rv')
  | si :: rest, nw', fl', rv' => do
      let (nw'', fl'', rv'') ← trsVStatementItem ctx cpos ifw isComb si nw'
      let nwAcc := nw'.merge nw''
      let flAcc := fl'.merge fl''
      let rvAcc := match rv'' with
        | some value => some value
        | none => rv'
      trsVStatementSeqBlock ctx cpos ifw isComb rest nwAcc flAcc rvAcc

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
      pure (nw.set p cv)
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
      pure (nw.set p cv)
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

def trsVModuleCommonItem (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (isComb : Bool) : module_common_item → NW → trsOk (NW × Flops)
  | .decl (.pkg pgid), nw => do
      let nw' ← trsVPkgGenItemDecl ctx cpos ifw nw pgid
      pure (nw', State.empty)
  | .cont_assign ca, nw => do
      let nw' ← trsVContAssign ctx cpos ifw nw ca
      pure (nw', State.empty)
  | .always ak (.stmt si), nw => do
      let alwaysComb ← alwaysIsComb ak
      let (nw', fl, _) ← trsVStatementItem ctx cpos ifw alwaysComb si nw
      pure (nw', fl)
  | .initial (.stmt si), nw => do
      let (nw', fl, _) ← trsVStatementItem ctx cpos ifw isComb si nw
      pure (nw', fl)
  | .assert _, nw => pure (nw, State.empty)

-- ## Module instantiation

private def trsVModuleInsMTrsArgsOne (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (nw : NW) (mtrs : MTrs) : named_port_conn →
      trsOk (List (Option Value))
  | .wildcard =>
      pure (mtrs.inputVids.map fun vid => trsToOption (wfind ctx ifw nw vid))
  | .ident pid =>
      pure (mtrs.inputVids.map fun vid =>
        if vid == pid then trsToOption (wfind ctx ifw nw pid) else none)
  | .expr pid e =>
      pure (mtrs.inputVids.map fun vid =>
        if vid == pid then trsToOption (evalExpr ctx cpos ifw nw e) else none)

def trsVModuleInsMTrsArgs (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (nw : NW) (mtrs : MTrs) : named_port_conns →
      trsOk (List (Option Value))
  | .one npc => trsVModuleInsMTrsArgsOne ctx cpos ifw nw mtrs npc
  | .cons npc npcs => do
      let args1 ← trsVModuleInsMTrsArgsOne ctx cpos ifw nw mtrs npc
      let args2 ← trsVModuleInsMTrsArgs ctx cpos ifw nw mtrs npcs
      pure (args1.zipWith (fun left right => match left with
        | some value => some value
        | none => right) args2)

def trsVModuleInsMTrs (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (ifw : IFW) (flops : Flops) : module_ins → NW → trsOk (NW × Flops)
  | .module mid _ (.hier iid (.named npcs)), nw => do
      let mtrs ← mtrss mid
      let args ← trsVModuleInsMTrsArgs ctx cpos ifw nw mtrs npcs
      let inputState := buildOptionalInputState mtrs.inputVids args
      let flopState := (flops.child? iid).getD State.empty
      let (newWires, newFlops) := mtrs.func inputState flopState
      -- write outputs to enclosing nw
      let nw' := mtrs.outputVids.foldl (fun acc ovid =>
        match newWires.get? ovid, hpos ovid ctx.decls.fields with
        | some ov, some p => acc.set p ov
        | _, _ => acc) nw
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
  | .module mogi, nw => trsVModuleOrGenerateItem ctx mtrss cpos ifw flops isComb mogi nw
  | .cond ce tgmi fgmi, nw => do
      let cv ← evalConst ctx.consts ce
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
      trsVModuleOrGenerateItem ctx mtrss cpos ifw flops isComb mogi nw

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

-- ## Fixed-point iteration (trsM_iff_rep)

-- Apply the IFF transition `n` times, feeding output back as input.
def trsM_iff_rep (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (m : module_decl) : Nat → IFF → trsOk IFF
  | 0, iff_ => pure iff_
  | n + 1, iff_ => do
      let iff' ← trsVModuleDecl_IFF ctx mtrss cpos m iff_
      trsM_iff_rep ctx mtrss cpos m n iff'

-- Build the final MTrs for a module: iterate until convergence (5 iterations).
def trsM_IFF (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (m : module_decl) : IFF → trsOk IFF :=
  trsM_iff_rep ctx mtrss cpos m 5

-- ## trsNext / trsT — compute next state and extract output

def trsNext (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (m : module_decl)
    (inputs : State) (flops : Flops) : trsOk (State × Flops) := do
  let ifw := flops.merge inputs
  let (ifw', flops') ← trsM_IFF ctx mtrss cpos m (ifw, flops)
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

def declsVModuleDecl (tdefs : TDefs) (consts : Consts) : module_decl → trsOk Decls
  | .ansi _ pps pdecls mitems => do
      let ppd ← declsVParamPorts tdefs consts pps
      let apd ← declsVAnsiPortDecls tdefs consts pdecls
      let mid ← declsVModuleItems tdefs consts mitems
      pure ⟨ppd ++ apd ++ mid⟩

-- ## Function collection

def funcsVFuncDecl (ctx : ModuleCtx) (cpos : HPath) :
    func_decl → trsOk (VId × Func)
  | .func _dti fid ports (.stmt si) => do
      -- Build input vid list from ports
      let inputVids := funcsPortVids ports
      -- Build the function
      let f : Func := {
        inputVids := inputVids
        func := fun inputState =>
          let ifw := ctx.decls.merge inputState
          match trsVStatementItem ctx cpos ifw true si State.empty with
          | .ok (_, _, rv) => rv
          | .error _ => none
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

def funcsVModuleDecl (decls : Decls) (consts : Consts) (cpos : HPath) : module_decl → trsOk Funcs
  | .ansi _ _ _ mitems => do
      let ctx : ModuleCtx := { decls, funcs := fmapEmpty, consts }
      let funcList ← funcsVModuleItems ctx cpos mitems
      pure (funcList.foldl (fun acc (vid, f) => fmapMerge (fmapSingle vid f) acc) fmapEmpty)

def moduleCtxVModuleDecl (cpos : HPath) (m : module_decl) : trsOk ModuleCtx := do
  let consts0 ← computeConsts m
  let (tdefs, consts) ← computeTDefs consts0 m
  let decls ← declsVModuleDecl tdefs consts m
  let funcs ← funcsVModuleDecl decls consts cpos m
  pure { decls, funcs, consts }

-- ## Parameter ports — with module-level context

def declsVParamPortsM (m : module_decl) : trsOk (List (VId × Value)) := do
  let consts0 ← computeConsts m
  let (tdefs, consts) ← computeTDefs consts0 m
  match m with
  | .ansi _ pps _ _ => declsVParamPorts tdefs consts pps

end VerilLean.Lang.Semantics
