/- # Module Flattening.
   Proves that hierarchical module instance evaluation via MTrs is equivalent
   to direct inline evaluation when the MTrs is faithful. -/

import VerilLean.Lib.Lib
import VerilLean.Lang.Syntax
import VerilLean.Lang.Semantics
import VerilLean.Lang.Equiv.MTrsFaithful

namespace VerilLean.Lang.Equiv.Flatten

open VerilLean.Lang.Syntax
open VerilLean.Lang.Semantics
open VerilLean.Lang.Equiv.MTrsFaithful (MTrsFaithful)
open VerilLean.Lib

-- ## Inline module instance evaluation

/- Evaluate a module instance by directly running the submodule's concrete
   semantics (trsM_IFF) instead of calling mtrs.func.

   This is the "flattened" version of trsVModuleInsMTrs: instead of treating
   the submodule as a black box (mtrs.func), we evaluate it concretely via
   trsM_IFF on the submodule's module_decl. -/
def trsVModuleInsInline
    -- Parent context
    (decls : Decls) (funcs : Funcs) (mtrss : MTrss)
    (ctxs : State) (cpos : HPath) (ifw : IFW) (flops : Flops)
    -- Submodule context
    (submod : module_decl)
    (sub_decls : Decls) (sub_funcs : Funcs)
    (sub_ctxs : State) (sub_cpos : HPath)
    -- MTrs (needed for inputVids, outputVids, and arg evaluation)
    (mtrs : MTrs)
    -- Instance info
    (iid : VId) (npcs : named_port_conns) (nw : NW)
    : trsOk (NW × Flops) := do
  let args ← trsVModuleInsMTrsArgs decls funcs ctxs cpos ifw nw mtrs npcs
  let inputState := buildFInputState mtrs.inputVids args
  let flopState := haccess flops iid
  let (newWires, newFlops) ← trsM_IFF mtrss sub_ctxs sub_cpos sub_decls sub_funcs submod
    (hupds flopState inputState, flopState)
  let nw' := mtrs.outputVids.foldl (fun acc ovid =>
    let ov := haccess newWires ovid
    match hpos ovid (hstr decls) with
    | some p => hadd acc p ov
    | none => acc) nw
  pure (nw', HMap.str [(iid, newFlops)])

-- ## Main equivalence theorem

/- When the submodule's MTrs is faithful, hierarchical evaluation via
   trsVModuleInsMTrs (which calls mtrs.func) is equivalent to inline
   evaluation via trsVModuleInsInline (which calls trsM_IFF directly). -/
theorem moduleIns_faithful_equiv
    -- Parent context
    (decls : Decls) (funcs : Funcs) (mtrss : MTrss)
    (ctxs : State) (cpos : HPath) (ifw : IFW) (flops : Flops)
    -- Module instance info
    (mid : VId) (pva : param_value_assigns) (iid : VId)
    (npcs : named_port_conns) (nw : NW)
    -- Submodule info
    (mtrs : MTrs) (submod : module_decl)
    (sub_decls : Decls) (sub_funcs : Funcs)
    (sub_ctxs : State) (sub_cpos : HPath)
    -- Hypotheses
    (hmtrss : mtrss mid = .ok mtrs)
    (hfaith : MTrsFaithful mtrs submod sub_decls sub_funcs mtrss sub_ctxs sub_cpos) :
    trsVModuleInsMTrs decls funcs mtrss ctxs cpos ifw flops
      (.module mid pva (.hier iid (.named npcs))) nw =
    trsVModuleInsInline decls funcs mtrss ctxs cpos ifw flops
      submod sub_decls sub_funcs sub_ctxs sub_cpos mtrs iid npcs nw := by
  simp only [trsVModuleInsMTrs, trsVModuleInsInline, hmtrss,
             bind, Except.bind, pure, Except.pure]
  cases h : trsVModuleInsMTrsArgs decls funcs ctxs cpos ifw nw mtrs npcs with
  | error e => rfl
  | ok args =>
    simp only []
    have hf := hfaith (buildFInputState mtrs.inputVids args) (haccess flops iid)
    simp only [hf]
    rfl

end VerilLean.Lang.Equiv.Flatten
