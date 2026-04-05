/- # MTrs Faithfulness.
   Defines when an MTrs correctly represents a module's semantics,
   and provides the building blocks for hierarchical correctness proofs. -/

import VerilLean.Lib.Lib
import VerilLean.Lang.Syntax
import VerilLean.Lang.Semantics

namespace VerilLean.Lang.Equiv.MTrsFaithful

open VerilLean.Lang.Syntax
open VerilLean.Lang.Semantics
open VerilLean.Lib

-- ## Module environment

/- A module environment maps module names to their declarations and
   associated evaluation context. This captures the global module namespace
   that would be available during elaboration. -/
structure ModuleEnv where
  modDecl : VId -> Option module_decl
  modDecls : VId -> Decls
  modFuncs : VId -> Funcs
  modCtxs : VId -> State
  modCpos : VId -> HPath

-- ## MTrs Faithfulness

/- An MTrs is faithful to a module declaration if `mtrs.func` produces
   the same result as evaluating the module via `trsM_IFF` (5 iterations
   of the combinational fixed-point computation).

   For all input and flop states, `trsM_IFF` succeeds and its result
   equals `mtrs.func inputState flopState`.

   The `mtrss` parameter represents the sub-module transition functions
   used during evaluation -- faithfulness is relative to these. -/
def MTrsFaithful (mtrs : MTrs) (mod : module_decl)
    (decls : Decls) (funcs : Funcs) (mtrss : MTrss)
    (ctxs : State) (cpos : HPath) : Prop :=
  forall (inputState flopState : State),
    trsM_IFF mtrss ctxs cpos decls funcs mod
      (hupds flopState inputState, flopState) =
    .ok (mtrs.func inputState flopState)

/- All MTrs in a map are faithful to their respective module declarations. -/
def AllMTrsFaithful (mtrss : MTrss) (env : ModuleEnv) : Prop :=
  forall (mid : VId) (mtrs : MTrs) (mod : module_decl),
    mtrss mid = .ok mtrs ->
    env.modDecl mid = some mod ->
    MTrsFaithful mtrs mod (env.modDecls mid) (env.modFuncs mid) mtrss
      (env.modCtxs mid) (env.modCpos mid)

-- ## Evaluation success

/- A module evaluates successfully for all inputs: `trsM_IFF` never errors. -/
def ModuleEvalOk (mod : module_decl) (decls : Decls) (funcs : Funcs)
    (mtrss : MTrss) (ctxs : State) (cpos : HPath) : Prop :=
  forall (ifw : IFW) (flops : Flops),
    exists ifw' flops',
      trsM_IFF mtrss ctxs cpos decls funcs mod (ifw, flops) = .ok (ifw', flops')

-- ## Constructing faithful MTrs

/- Build an MTrs from a module declaration and sub-module MTrs.
   The resulting `func` runs `trsM_IFF` and extracts the result.
   If evaluation errors, returns empty states (excluded by `ModuleEvalOk`). -/
def buildMTrs (mod : module_decl) (decls : Decls) (funcs : Funcs)
    (mtrss : MTrss) (ctxs : State) (cpos : HPath)
    (inputVids outputVids : List VId) : MTrs :=
  { inputVids := inputVids
    outputVids := outputVids
    func := fun inputState flopState =>
      match trsM_IFF mtrss ctxs cpos decls funcs mod
        (hupds flopState inputState, flopState) with
      | .ok r => r
      | .error _ => (HMap.empty, HMap.empty) }

/- An MTrs built via `buildMTrs` is faithful when evaluation succeeds. -/
theorem buildMTrs_faithful
    (mod : module_decl) (decls : Decls) (funcs : Funcs)
    (mtrss : MTrss) (ctxs : State) (cpos : HPath)
    (inputVids outputVids : List VId)
    (hOk : ModuleEvalOk mod decls funcs mtrss ctxs cpos) :
    MTrsFaithful (buildMTrs mod decls funcs mtrss ctxs cpos inputVids outputVids)
      mod decls funcs mtrss ctxs cpos := by
  intro inputState flopState
  simp only [buildMTrs]
  obtain ⟨ifw', flops', heval⟩ := hOk (hupds flopState inputState) flopState
  simp only [heval]

/- Symmetric form: from faithfulness, extract the func equality. -/
theorem faithful_func_eq
    (mtrs : MTrs) (mod : module_decl)
    (decls : Decls) (funcs : Funcs) (mtrss : MTrss)
    (ctxs : State) (cpos : HPath)
    (hfaith : MTrsFaithful mtrs mod decls funcs mtrss ctxs cpos)
    (inputState flopState : State) :
    .ok (mtrs.func inputState flopState) =
    trsM_IFF mtrss ctxs cpos decls funcs mod
      (hupds flopState inputState, flopState) :=
  (hfaith inputState flopState).symm

/- Faithfulness implies evaluation success. -/
theorem faithful_implies_evalOk
    (mtrs : MTrs) (mod : module_decl)
    (decls : Decls) (funcs : Funcs) (mtrss : MTrss)
    (ctxs : State) (cpos : HPath)
    (hfaith : MTrsFaithful mtrs mod decls funcs mtrss ctxs cpos) :
    forall (inputState flopState : State),
      exists ifw' flops',
        trsM_IFF mtrss ctxs cpos decls funcs mod
          (hupds flopState inputState, flopState) = .ok (ifw', flops') := by
  intro inputState flopState
  have h := hfaith inputState flopState
  exact ⟨(mtrs.func inputState flopState).1,
         (mtrs.func inputState flopState).2,
         by rw [h]⟩

/- Faithfulness gives us the concrete result values. -/
theorem faithful_result
    (mtrs : MTrs) (mod : module_decl)
    (decls : Decls) (funcs : Funcs) (mtrss : MTrss)
    (ctxs : State) (cpos : HPath)
    (hfaith : MTrsFaithful mtrs mod decls funcs mtrss ctxs cpos)
    (inputState flopState : State) :
    exists ifw' flops',
      trsM_IFF mtrss ctxs cpos decls funcs mod
        (hupds flopState inputState, flopState) = .ok (ifw', flops') ∧
      mtrs.func inputState flopState = (ifw', flops') := by
  have h := hfaith inputState flopState
  exact ⟨(mtrs.func inputState flopState).1,
         (mtrs.func inputState flopState).2,
         h, by simp⟩

end VerilLean.Lang.Equiv.MTrsFaithful
