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
abbrev State := Value
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
  func      : State → Value

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
def hfind2 (p : HPath) (h1 h2 : HMap) : Option HMap :=
  let v := hfind h1 p
  if v == HMap.empty then
    let v2 := hfind h2 p
    if v2 == HMap.empty then none else some v2
  else some v

-- Build a state from parallel lists of variable ids and values.
def buildFInputState (vids : List VId) (args : List Value) : State :=
  HMap.str (vids.zip args)

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

-- ## Constant-expression evaluation

abbrev Consts := HMap

structure ModuleCtx where
  decls  : Decls
  funcs  : Funcs
  consts : Consts

def cfind (consts : Consts) (vid : VId) : trsOk Value :=
  match haccessO consts vid with
  | some v => .ok v
  | none => .error .undeclared

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
      pure (hselect tv (hbits sv))
  | .select_const_range se lr rr => do
      let sv ← evalConst consts se
      let lv ← evalConst consts lr
      let rv ← evalConst consts rr
      pure (hrange sv (hbits lv) (hbits rv))
  | .select_indexed_range_add se lr rr => do
      let sv ← evalConst consts se
      let lv ← evalConst consts lr
      let rv ← evalConst consts rr
      let lsz := hbits lv
      let rsz := hbits rv
      let hi := SZ.bAdd lsz (SZ.bSub rsz (SZ.mk' 1 rsz.width false))
      pure (hrange sv hi lsz)
  | .select_indexed_range_sub se lr rr => do
      let sv ← evalConst consts se
      let lv ← evalConst consts lr
      let rv ← evalConst consts rr
      let lsz := hbits lv
      let rsz := hbits rv
      let lo := SZ.bSub lsz (SZ.bSub rsz (SZ.mk' 1 rsz.width false))
      pure (hrange sv lsz lo)
  | .concat es => do
      let vs ← evalConstList consts es
      pure (harray vs)
  | .mult_concat ne ces => do
      let nv ← evalConst consts ne
      let cvs ← evalConstList consts ces
      let count := (hbits nv).norm.toNat
      let repeated := (List.replicate count cvs).flatten
      pure (harray repeated)
  | .tf_call _ _ => .error .notSupported
  | .system_tf_call .signed aes =>
      match aes with
      | [ae] => do let av ← evalConst consts ae; pure (HMap.bits (hbits av).toSigned)
      | _ => .error .notSupported
  | .system_tf_call .unsigned aes =>
      match aes with
      | [ae] => do let av ← evalConst consts ae; pure (HMap.bits (hbits av).toUnsigned)
      | _ => .error .notSupported
  | .cast sze e => do
      let szv ← evalConst consts sze
      let ev ← evalConst consts e
      pure (HMap.bits (SZ.castV (hbits szv) (hbits ev)))
  | .unary_op op e => do
      let ev ← evalConst consts e
      pure (HMap.bits (uniOpFunc op (hbits ev)))
  | .inc_or_dec _ => .error .notSupported
  | .binary_op op le re => do
      let lv ← evalConst consts le
      let rv ← evalConst consts re
      pure (HMap.bits (binOpFunc op (hbits lv) (hbits rv)))
  | .cond ce te fe => do
      let cv ← evalConst consts ce
      let tv ← evalConst consts te
      let fv ← evalConst consts fe
      if (hbits cv).isZero then pure fv else pure tv
  | .inside ie res => do
      let iv ← evalConst consts ie
      let rvs ← evalConstList consts res
      let isz := hbits iv
      let isMatch := rvs.any (fun rv => SZ.equiv isz (hbits rv))
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
      let lsz := hbits lv
      let rsz := hbits rv
      let w := (lsz.norm - rsz.norm).toNat + 1
      pure (some (HMap.bits (SZ.mk' 0 w false)))
  | .one (.one de) => do
      let dv ← evalConst consts de
      let dsz := hbits dv
      let w := dsz.norm.toNat
      pure (some (HMap.bits (SZ.mk' 0 w false)))
  | .cons pd pds => do
      let _ ← match pd with
        | .range lr rr => do
            let lv ← evalConst consts lr
            let rv ← evalConst consts rr
            let lsz := hbits lv
            let rsz := hbits rv
            let w := (lsz.norm - rsz.norm).toNat + 1
            pure (some (HMap.bits (SZ.mk' 0 w false)))
        | .one de => do
            let dv ← evalConst consts de
            let dsz := hbits dv
            let w := dsz.norm.toNat
            pure (some (HMap.bits (SZ.mk' 0 w false)))
      evalPackedDims consts pds

-- Get the default value for a data type.
def declDataType (consts : Consts) : data_type → trsOk Value
  | .int_vec _ pds => do
      let ov ← evalPackedDims consts pds
      match ov with
      | some v => pure v
      | none => pure (HMap.bits (SZ.mk' 0 1 false))
  | .int_atom .byte => pure (HMap.bits (SZ.mk' 0 8 false))
  | .int_atom .short_int => pure (HMap.bits (SZ.mk' 0 16 true))
  | .int_atom .long_int => pure (HMap.bits (SZ.mk' 0 64 true))
  | .int_atom .integer => pure (HMap.bits (SZ.mk' 0 32 true))
  | .int_atom .time => pure (HMap.bits (SZ.mk' 0 64 false))

def declDataTypeOrImplicit (consts : Consts) : data_type_or_implicit → trsOk Value
  | .data dt => declDataType consts dt
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

-- ## Parameter value collection

def paramValue (consts : Consts) (dti : data_type_or_implicit) (ce : constant_expression) :
    trsOk Value := do
  let v ← evalConst consts ce
  match dti with
  | .implicit .nil => pure v
  | _ => do
      let dv ← declDataTypeOrImplicit consts dti
      pure (HMap.bits (SZ.castD (hbits dv) (hbits v)))

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
      let env0 ← collectParamPortValues HMap.empty pps
      collectParamValues mitems env0

-- ## declfind / wfind — looking up declarations and values

-- Find the path to a variable in declarations.
def declfind (decls : Decls) (vid : VId) : trsOk HPath :=
  liftOption (hpos vid (hstr decls)) .undeclared

-- Find a variable value: look in nw, then ifw, then consts.
def wfind (ctx : ModuleCtx) (ifw : IFW) (nw : NW) (vid : VId) : trsOk Value := do
  let p ← declfind ctx.decls vid
  match hfind2 p nw ifw with
  | some v => pure v
  | none =>
      match haccessO ctx.consts vid with
      | some v => pure v
      | none => .error .undriven

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
      pure (hselect tv (hbits sv))
  | .select_const_range se lr rr => do
      let sv ← evalExpr ctx cpos ifw nw se
      let lv ← evalExpr ctx cpos ifw nw lr
      let rv ← evalExpr ctx cpos ifw nw rr
      pure (hrange sv (hbits lv) (hbits rv))
  | .select_indexed_range_add se lr rr => do
      let sv ← evalExpr ctx cpos ifw nw se
      let lv ← evalExpr ctx cpos ifw nw lr
      let rv ← evalExpr ctx cpos ifw nw rr
      let lsz := hbits lv
      let rsz := hbits rv
      let hi := SZ.bAdd lsz (SZ.bSub rsz (SZ.mk' 1 rsz.width false))
      pure (hrange sv hi lsz)
  | .select_indexed_range_sub se lr rr => do
      let sv ← evalExpr ctx cpos ifw nw se
      let lv ← evalExpr ctx cpos ifw nw lr
      let rv ← evalExpr ctx cpos ifw nw rr
      let lsz := hbits lv
      let rsz := hbits rv
      let lo := SZ.bSub lsz (SZ.bSub rsz (SZ.mk' 1 rsz.width false))
      pure (hrange sv lsz lo)
  | .concat es => do
      let vs ← evalExprList ctx cpos ifw nw es
      pure (harray vs)
  | .mult_concat ne ces => do
      let nv ← evalConst ctx.consts ne
      let cvs ← evalExprList ctx cpos ifw nw ces
      let count := (hbits nv).norm.toNat
      let repeated := (List.replicate count cvs).flatten
      pure (harray repeated)
  | .tf_call tfid aes => do
      let f ← ctx.funcs tfid
      let avs ← evalExprList ctx cpos ifw nw aes
      let inputState := buildFInputState f.inputVids avs
      pure (f.func inputState)
  | .system_tf_call .signed aes =>
      match aes with
      | [ae] => do
          let av ← evalExpr ctx cpos ifw nw ae
          pure (HMap.bits (hbits av).toSigned)
      | _ => .error .notSupported
  | .system_tf_call .unsigned aes =>
      match aes with
      | [ae] => do
          let av ← evalExpr ctx cpos ifw nw ae
          pure (HMap.bits (hbits av).toUnsigned)
      | _ => .error .notSupported
  | .cast sze e => do
      let szv ← evalExpr ctx cpos ifw nw sze
      let ev ← evalExpr ctx cpos ifw nw e
      pure (HMap.bits (SZ.castV (hbits szv) (hbits ev)))
  | .unary_op op e => do
      let ev ← evalExpr ctx cpos ifw nw e
      pure (HMap.bits (uniOpFunc op (hbits ev)))
  | .inc_or_dec (.inc vid) => do
      let v ← wfind ctx ifw nw vid
      pure (HMap.bits (SZ.bAdd (hbits v) (SZ.mk' 1 (hbits v).width false)))
  | .inc_or_dec (.dec vid) => do
      let v ← wfind ctx ifw nw vid
      pure (HMap.bits (SZ.bSub (hbits v) (SZ.mk' 1 (hbits v).width false)))
  | .binary_op op le re => do
      let lv ← evalExpr ctx cpos ifw nw le
      let rv ← evalExpr ctx cpos ifw nw re
      pure (HMap.bits (binOpFunc op (hbits lv) (hbits rv)))
  | .cond ce te fe => do
      let cv ← evalExpr ctx cpos ifw nw ce
      let tv ← evalExpr ctx cpos ifw nw te
      let fv ← evalExpr ctx cpos ifw nw fe
      if (hbits cv).isZero then pure fv else pure tv
  | .inside ie res => do
      let iv ← evalExpr ctx cpos ifw nw ie
      let rvs ← evalExprList ctx cpos ifw nw res
      let isz := hbits iv
      let isMatch := rvs.any (fun rv => SZ.equiv isz (hbits rv))
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
      pure (pp ++ [HElt.ind (hbits sv)])
  | _ => .error .notSupported

end

-- ## nfupds / pnfupds — state update helpers

-- Update (nw, flops) triple using result of an assignment.
def nfupds (nfr1 nfr2 : NW × Flops) : NW × Flops :=
  (hupds nfr1.1 nfr2.1, hupds nfr1.2 nfr2.2)

-- Predicate-filtered update of (nw, flops).
def pnfupds (p : VId → Bool) (nfr1 nfr2 : NW × Flops) : NW × Flops :=
  (phupds p nfr1.1 nfr2.1, phupds p nfr1.2 nfr2.2)

-- ## Assignment processing

def trsVAssign (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (nw : NW) : assign → trsOk NW
  | .net lv e => do
      let p ← lvposfind ctx cpos ifw nw lv
      let v ← evalExpr ctx cpos ifw nw e
      let dv := hfind ctx.decls p
      let cv := HMap.bits (SZ.castD (hbits dv) (hbits v))
      pure (hadd nw p cv)

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
      let dv := hfind ctx.decls p
      match assignOpToBinOp aop with
      | none =>
          let cv := HMap.bits (SZ.castD (hbits dv) (hbits ev))
          pure (hadd nw p cv)
      | some bop => do
          let lval ← match hfind2 p nw ifw with
            | some v => pure v
            | none => .error .undriven
          let result := binOpFunc bop (hbits lval) (hbits ev)
          let cv := HMap.bits (SZ.castD (hbits dv) result)
          pure (hadd nw p cv)
  | .inc_or_dec (.inc vid) => do
      let p ← declfind ctx.decls vid
      let v ← wfind ctx ifw nw vid
      let dv := hfind ctx.decls p
      let result := SZ.bAdd (hbits v) (SZ.mk' 1 (hbits v).width false)
      let cv := HMap.bits (SZ.castD (hbits dv) result)
      pure (hadd nw p cv)
  | .inc_or_dec (.dec vid) => do
      let p ← declfind ctx.decls vid
      let v ← wfind ctx ifw nw vid
      let dv := hfind ctx.decls p
      let result := SZ.bSub (hbits v) (SZ.mk' 1 (hbits v).width false)
      let cv := HMap.bits (SZ.castD (hbits dv) result)
      pure (hadd nw p cv)

-- ## Statement execution (mutual recursion)

mutual

/- Main statement interpreter.
   Returns (new wire values, flop updates, return value). -/
def trsVStatementItem (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (isComb : Bool) : statement_item → NW → trsOk (NW × Flops × Value)
  | .blocking_assign_normal lv e, nw => do
      let p ← lvposfind ctx cpos ifw nw lv
      let v ← evalExpr ctx cpos ifw nw e
      let dv := hfind ctx.decls p
      let cv := HMap.bits (SZ.castD (hbits dv) (hbits v))
      pure (hadd nw p cv, HMap.empty, HMap.empty)
  | .nonblocking_assign lv e, nw => do
      let p ← lvposfind ctx cpos ifw nw lv
      let v ← evalExpr ctx cpos ifw nw e
      let dv := hfind ctx.decls p
      let cv := HMap.bits (SZ.castD (hbits dv) (hbits v))
      if isComb
        then pure (hadd nw p cv, HMap.empty, HMap.empty)
        else pure (nw, hsingle p cv, HMap.empty)
  | .case _ ce css, nw => do
      let cv ← evalExpr ctx cpos ifw nw ce
      trsVStatementCaseV ctx cpos ifw isComb cv css nw
  | .cond cp ts fs, nw => do
      let cv ← evalExpr ctx cpos ifw nw cp
      if (hbits cv).isZero then
        match fs with
        | none => pure (nw, HMap.empty, HMap.empty)
        | some none => pure (nw, HMap.empty, HMap.empty)
        | some (some fsi) => trsVStatementItem ctx cpos ifw isComb fsi nw
      else
        match ts with
        | none => pure (nw, HMap.empty, HMap.empty)
        | some tsi => trsVStatementItem ctx cpos ifw isComb tsi nw
  | .forever _, nw => pure (nw, HMap.empty, HMap.empty)  -- skip
  | .repeat _ _, nw => pure (nw, HMap.empty, HMap.empty)  -- skip
  | .while _ _, nw => pure (nw, HMap.empty, HMap.empty)  -- skip
  | .do_while _ _, nw => pure (nw, HMap.empty, HMap.empty)  -- skip
  | .for (.var_assigns fias) ce step body, nw => do
      let nw' ← trsVAssigns ctx cpos ifw nw fias
      trsVStatementForLoop ctx cpos ifw isComb
        ce step body 32 nw' HMap.empty HMap.empty
  | .return re, nw => do
      let rv ← evalExpr ctx cpos ifw nw re
      pure (nw, HMap.empty, rv)
  | .proc_timing_control _ si, nw =>
      trsVStatementItem ctx cpos ifw isComb si nw
  | .seq_block stis, nw =>
      trsVStatementSeqBlock ctx cpos ifw isComb stis nw HMap.empty HMap.empty

-- Process a case statement: find matching case item and execute.
def trsVStatementCaseV (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (isComb : Bool)
    (cv : Value) : List (case_item statement_item) → NW → trsOk (NW × Flops × Value)
  | [], nw => pure (nw, HMap.empty, HMap.empty)
  | (.default st) :: _, nw => trsVStatementItem ctx cpos ifw isComb st nw
  | (.case ce st) :: rest, nw => do
      let cev ← evalExpr ctx cpos ifw nw ce
      if SZ.equiv (hbits cv) (hbits cev)
        then trsVStatementItem ctx cpos ifw isComb st nw
        else trsVStatementCaseV ctx cpos ifw isComb cv rest nw

-- Evaluate a for-loop with bounded unrolling (max 2^5 = 32 iterations).
def trsVStatementForLoop (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (isComb : Bool)
    (ce : expression) (step : for_step) (body : statement_item)
    (fuel : Nat) (nw : NW) (flops : Flops) (retv : Value) : trsOk (NW × Flops × Value) :=
  match fuel with
  | 0 => pure (nw, flops, retv)
  | fuel' + 1 => do
    let cv ← evalExpr ctx cpos ifw nw ce
    if (hbits cv).isZero
      then pure (nw, flops, retv)
      else do
        let (nw', fl', rv') ← trsVStatementItem ctx cpos ifw isComb body nw
        let nw'' := hupds nw nw'
        let flops' := hupds flops fl'
        let retv' := if rv' == HMap.empty then retv else rv'
        -- apply step
        let nw''' ← trsVForStep ctx cpos ifw nw'' step
        trsVStatementForLoop ctx cpos ifw isComb ce step body
          fuel' nw''' flops' retv'

-- Execute a sequence of statements.
def trsVStatementSeqBlock (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (isComb : Bool) : List statement_item → NW → Flops → Value → trsOk (NW × Flops × Value)
  | [], nw', fl', rv' => pure (nw', fl', rv')
  | si :: rest, nw', fl', rv' => do
      let (nw'', fl'', rv'') ← trsVStatementItem ctx cpos ifw isComb si nw'
      let nwAcc := hupds nw' nw''
      let flAcc := hupds fl' fl''
      let rvAcc := if rv'' == HMap.empty then rv' else rv''
      trsVStatementSeqBlock ctx cpos ifw isComb rest nwAcc flAcc rvAcc

end

-- ## Declaration assignment processing

def trsVNetDeclAssign (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (nw : NW) : net_decl_assign → trsOk NW
  | .net vid (some e) => do
      let p ← declfind ctx.decls vid
      let v ← evalExpr ctx cpos ifw nw e
      let dv := hfind ctx.decls p
      let cv := HMap.bits (SZ.castD (hbits dv) (hbits v))
      pure (hadd nw p cv)
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
      let dv := hfind ctx.decls p
      let cv := HMap.bits (SZ.castD (hbits dv) (hbits v))
      pure (hadd nw p cv)
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

def trsVModuleCommonItem (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (isComb : Bool) : module_common_item → NW → trsOk (NW × Flops)
  | .decl (.pkg pgid), nw => do
      let nw' ← trsVPkgGenItemDecl ctx cpos ifw nw pgid
      pure (nw', HMap.empty)
  | .cont_assign ca, nw => do
      let nw' ← trsVContAssign ctx cpos ifw nw ca
      pure (nw', HMap.empty)
  | .always _ (.stmt si), nw => do
      let (nw', fl, _) ← trsVStatementItem ctx cpos ifw isComb si nw
      pure (nw', fl)
  | .initial (.stmt si), nw => do
      let (nw', fl, _) ← trsVStatementItem ctx cpos ifw isComb si nw
      pure (nw', fl)
  | .assert _, nw => pure (nw, HMap.empty)

-- ## Module instantiation

private def trsVModuleInsMTrsArgsOne (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (nw : NW) (mtrs : MTrs) : named_port_conn → trsOk (List Value)
  | .wildcard =>
      sfListMap (fun vid => sfOrReturn (wfind ctx ifw nw vid) (pure HMap.empty) pure)
        mtrs.inputVids
  | .ident pid =>
      sfListMap (fun vid =>
        if vid == pid then sfOrReturn (wfind ctx ifw nw pid) (pure HMap.empty) pure
        else pure HMap.empty) mtrs.inputVids
  | .expr pid e =>
      sfListMap (fun vid =>
        if vid == pid then sfOrReturn (evalExpr ctx cpos ifw nw e) (pure HMap.empty) pure
        else pure HMap.empty) mtrs.inputVids

def trsVModuleInsMTrsArgs (ctx : ModuleCtx) (cpos : HPath)
    (ifw : IFW) (nw : NW) (mtrs : MTrs) : named_port_conns → trsOk (List Value)
  | .one npc => trsVModuleInsMTrsArgsOne ctx cpos ifw nw mtrs npc
  | .cons npc npcs => do
      let args1 ← trsVModuleInsMTrsArgsOne ctx cpos ifw nw mtrs npc
      let args2 ← trsVModuleInsMTrsArgs ctx cpos ifw nw mtrs npcs
      pure (args1.zipWith hupds args2)

def trsVModuleInsMTrs (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (ifw : IFW) (flops : Flops) : module_ins → NW → trsOk (NW × Flops)
  | .module mid _ (.hier iid (.named npcs)), nw => do
      let mtrs ← mtrss mid
      let args ← trsVModuleInsMTrsArgs ctx cpos ifw nw mtrs npcs
      let inputState := buildFInputState mtrs.inputVids args
      let flopState := haccess flops iid
      let (newWires, newFlops) := mtrs.func inputState flopState
      -- write outputs to enclosing nw
      let nw' := mtrs.outputVids.foldl (fun acc ovid =>
        let ov := haccess newWires ovid
        match hpos ovid (hstr ctx.decls) with
        | some p => hadd acc p ov
        | none => acc) nw
      pure (nw', HMap.str [(iid, newFlops)])

def trsVModuleIns (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (ifw : IFW) (flops : Flops) : module_ins → NW → trsOk (NW × Flops)
  | mi, nw => trsVModuleInsMTrs ctx mtrss cpos ifw flops mi nw

def trsVModuleOrGenerateItem (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (ifw : IFW) (flops : Flops) (isComb : Bool) : module_or_generate_item → NW → trsOk (NW × Flops)
  | .common ci, nw => trsVModuleCommonItem ctx cpos ifw isComb ci nw
  | .ins mi, nw => trsVModuleIns ctx mtrss cpos ifw flops mi nw

-- ## iffupds — update IFW and flops

def iffupds (iff1 iff2 : IFF) : IFF :=
  (hupds iff1.1 iff2.1, hupds iff1.2 iff2.2)

-- ## Generate module items

mutual
def trsVGenerateModuleItem (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (ifw : IFW) (flops : Flops) (isComb : Bool) : generate_module_item → NW → trsOk (NW × Flops)
  | .module mogi, nw => trsVModuleOrGenerateItem ctx mtrss cpos ifw flops isComb mogi nw
  | .cond ce tgmi fgmi, nw => do
      let cv ← evalConst ctx.consts ce
      if (hbits cv).isZero then
        match fgmi with
        | none => pure (nw, HMap.empty)
        | some fgmi' => trsVGenerateModuleItem ctx mtrss cpos ifw flops isComb fgmi' nw
      else
        trsVGenerateModuleItem ctx mtrss cpos ifw flops isComb tgmi nw
  | .block gmis, nw =>
      trsVGenerateModuleItemList ctx mtrss cpos ifw flops isComb gmis nw

def trsVGenerateModuleItemList (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (ifw : IFW) (flops : Flops) (isComb : Bool) : List generate_module_item → NW → trsOk (NW × Flops)
  | [], _ => .ok (HMap.empty, HMap.empty)
  | gmi :: rest, nw => do
      let b ← trsVGenerateModuleItem ctx mtrss cpos ifw flops isComb gmi nw
      let nw' := hupds nw b.1
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
  | .port_decl _, nw => pure (nw, HMap.empty)
  | .non_port np, nw => trsVNonPortModuleItem ctx mtrss cpos ifw flops isComb np nw

def trsVModuleItems (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (ifw : IFW) (flops : Flops) (isComb : Bool) : module_items → NW → trsOk (NW × Flops)
  | .one mi, nw => trsVModuleItem ctx mtrss cpos ifw flops isComb mi nw
  | .cons mi mis, nw => do
      let (nw', fl') ← trsVModuleItem ctx mtrss cpos ifw flops isComb mi nw
      let (nw'', fl'') ← trsVModuleItems ctx mtrss cpos ifw flops isComb mis (hupds nw nw')
      pure (hupds nw' nw'', hupds fl' fl'')

-- ## Module declaration — building the transition function

def trsVModuleDecl (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (m : module_decl) : State → State → trsOk (NW × Flops) :=
  match m with
  | .ansi _ _ _ mitems => fun ifw flops =>
      trsVModuleItems ctx mtrss cpos ifw flops true mitems HMap.empty

-- Build the IFF (combined IFW × Flops) transition.
def trsVModuleDecl_IFF (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (m : module_decl) : IFF → trsOk IFF :=
  fun (ifw, flops) => do
    let (nw, fl) ← trsVModuleDecl ctx mtrss cpos m ifw flops
    pure (hupds ifw nw, hupds flops fl)

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
  let ifw := hupds flops inputs
  let (ifw', flops') ← trsM_IFF ctx mtrss cpos m (ifw, flops)
  pure (ifw', flops')

def trsT (ctx : ModuleCtx) (mtrss : MTrss) (cpos : HPath)
    (m : module_decl)
    (outputVids : List VId) (inputs : State) (flops : Flops) : trsOk (State × Flops) := do
  let (ifw', flops') ← trsNext ctx mtrss cpos m inputs flops
  pure (hfilter outputVids ifw', flops')

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

def declsVVarDeclAssign (consts : Consts) (dt : data_type) :
    var_decl_assign → trsOk (VId × Value)
  | .var vid vd _ => do
      let basev ← declDataType consts dt
      -- handle unpacked dimensions
      let dv ← match vd with
        | .nil => pure basev
        | _ => pure basev  -- simplified: unpacked dims not fully handled
      pure (vid, dv)

def declsVVarDeclAssigns (consts : Consts) (dt : data_type) :
    var_decl_assigns → trsOk (List (VId × Value))
  | .one vda => do let r ← declsVVarDeclAssign consts dt vda; pure [r]
  | .cons vda vdas => do
      let r ← declsVVarDeclAssign consts dt vda
      let rest ← declsVVarDeclAssigns consts dt vdas
      pure (r :: rest)

def declsVParamAssign (consts : Consts) (dti : data_type_or_implicit) :
    param_assign → trsOk (VId × Value)
  | .param pid _ => do
      let dv ← declDataTypeOrImplicit consts dti
      pure (pid, dv)

def declsVParamAssigns (consts : Consts) (dti : data_type_or_implicit) :
    param_assigns → trsOk (List (VId × Value))
  | .one pa => do let r ← declsVParamAssign consts dti pa; pure [r]
  | .cons pa pas => do
      let r ← declsVParamAssign consts dti pa
      let rest ← declsVParamAssigns consts dti pas
      pure (r :: rest)

def declsVPkgGenItemDecl (consts : Consts) : pkg_gen_item_decl → trsOk (List (VId × Value))
  | .net (.net _ pd ndas) => declsVNetDeclAssigns consts pd ndas
  | .data (.var_decl (.var dt vdas)) => declsVVarDeclAssigns consts dt vdas
  | .param (.data dti pas) => declsVParamAssigns consts dti pas
  | .local_param (.local dti pas) => declsVParamAssigns consts dti pas
  | _ => pure []

def declsVModuleCommonItem (consts : Consts) : module_common_item → trsOk (List (VId × Value))
  | .decl (.pkg pgid) => declsVPkgGenItemDecl consts pgid
  | _ => pure []

def declsVModuleOrGenerateItem (consts : Consts) : module_or_generate_item → trsOk (List (VId × Value))
  | .common ci => declsVModuleCommonItem consts ci
  | .ins _ => pure []

mutual
def declsVGenerateModuleItem (consts : Consts) : generate_module_item → trsOk (List (VId × Value))
  | .module mogi => declsVModuleOrGenerateItem consts mogi
  | .cond _ tgmi fgmi => do
      let td ← declsVGenerateModuleItem consts tgmi
      let fd ← match fgmi with
        | none => pure []
        | some fgmi' => declsVGenerateModuleItem consts fgmi'
      pure (td ++ fd)
  | .block gmis => declsVGenerateModuleItemList consts gmis

def declsVGenerateModuleItemList (consts : Consts) : List generate_module_item → trsOk (List (VId × Value))
  | [] => pure []
  | gmi :: rest => do
      let d ← declsVGenerateModuleItem consts gmi
      let rest' ← declsVGenerateModuleItemList consts rest
      pure (d ++ rest')
end

def declsVNonPortModuleItem (consts : Consts) : non_port_module_item → trsOk (List (VId × Value))
  | .generated_module_ins (.generated gmi) => declsVGenerateModuleItem consts gmi
  | .module_or_generate_item mogi => declsVModuleOrGenerateItem consts mogi

def declsVModuleItem (consts : Consts) : module_item → trsOk (List (VId × Value))
  | .port_decl _ => pure []
  | .non_port np => declsVNonPortModuleItem consts np

def declsVModuleItems (consts : Consts) : module_items → trsOk (List (VId × Value))
  | .one mi => declsVModuleItem consts mi
  | .cons mi mis => do
      let d ← declsVModuleItem consts mi
      let rest ← declsVModuleItems consts mis
      pure (d ++ rest)

def declsVParamDecl (consts : Consts) : param_decl → trsOk (List (VId × Value))
  | .data dti pas => declsVParamAssigns consts dti pas

def declsVParamPorts (consts : Consts) : param_ports → trsOk (List (VId × Value))
  | .nil => pure []
  | .one pd => declsVParamDecl consts pd
  | .cons pd pds => do
      let d ← declsVParamDecl consts pd
      let rest ← declsVParamPorts consts pds
      pure (d ++ rest)

def declsVAnsiPortDecl (consts : Consts) : ansi_port_decl → trsOk (List (VId × Value))
  | .net (some (.net _ pt)) pid => do
      let dv ← declPortType consts pt
      pure [(pid, dv)]
  | .net none pid => pure [(pid, HMap.bits (SZ.mk' 0 1 false))]
  | .var (some (.var _ dt)) pid => do
      let dv ← declDataType consts dt
      pure [(pid, dv)]
  | .var none pid => pure [(pid, HMap.bits (SZ.mk' 0 1 false))]

def declsVAnsiPortDecls (consts : Consts) : ansi_port_decls → trsOk (List (VId × Value))
  | .nil => pure []
  | .one apd => declsVAnsiPortDecl consts apd
  | .cons apd apds => do
      let d ← declsVAnsiPortDecl consts apd
      let rest ← declsVAnsiPortDecls consts apds
      pure (d ++ rest)

def declsVModuleDecl (consts : Consts) : module_decl → trsOk Decls
  | .ansi _ pps pdecls mitems => do
      let ppd ← declsVParamPorts consts pps
      let apd ← declsVAnsiPortDecls consts pdecls
      let mid ← declsVModuleItems consts mitems
      pure (HMap.str (ppd ++ apd ++ mid))

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
          let ifw := hupds ctx.decls inputState
          match trsVStatementItem ctx cpos ifw true si HMap.empty with
          | .ok (_, _, rv) => rv
          | .error _ => HMap.empty
      }
      pure (fid, f)
where
  funcsPortVids : ansi_port_decls → List VId
    | .nil => []
    | .one (.net _ pid) => [pid]
    | .one (.var _ pid) => [pid]
    | .cons (.net _ pid) rest => pid :: funcsPortVids rest
    | .cons (.var _ pid) rest => pid :: funcsPortVids rest

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
  let consts ← computeConsts m
  let decls ← declsVModuleDecl consts m
  let funcs ← funcsVModuleDecl decls consts cpos m
  pure { decls, funcs, consts }

-- ## Parameter ports — with module-level context

def declsVParamPortsM (m : module_decl) : trsOk (List (VId × Value)) := do
  let consts ← computeConsts m
  match m with
  | .ansi _ pps _ _ => declsVParamPorts consts pps

end VerilLean.Lang.Semantics
