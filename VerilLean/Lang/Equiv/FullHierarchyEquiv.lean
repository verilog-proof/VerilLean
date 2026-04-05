/- # Full Hierarchy Equivalence.
   End-to-end theorem: for a well-formed module hierarchy, the hierarchical
   STF (using bottom-up MTrs) is observably equivalent to the standard
   (event-driven) semantics. -/

import VerilLean.Lib.Lib
import VerilLean.Lib.HMapLemmas
import VerilLean.Lang.Syntax
import VerilLean.Lang.Semantics
import VerilLean.Lang.Equiv.StaticCheck
import VerilLean.Lang.Equiv.Standard
import VerilLean.Lang.Equiv.EvalFrame
import VerilLean.Lang.Equiv.Confluence
import VerilLean.Lang.Equiv.StfTopological
import VerilLean.Lang.Equiv.StdTopological
import VerilLean.Lang.Equiv.Equiv
import VerilLean.Lang.Equiv.Bridge
import VerilLean.Lang.Equiv.FullEquiv
import VerilLean.Lang.Equiv.RegisterEquiv
import VerilLean.Lang.Equiv.MTrsFaithful
import VerilLean.Lang.Equiv.Flatten
import VerilLean.Lang.Equiv.HierarchyEquiv

namespace VerilLean.Lang.Equiv.FullHierarchyEquiv

open VerilLean.Lang.Syntax
open VerilLean.Lang.Semantics
open VerilLean.Lang.Equiv.Standard (ExecProcess StdReachesQuiet)
open VerilLean.Lang.Equiv.Confluence (StateEquiv)
open VerilLean.Lang.Equiv (ModuleWellFormed)
open VerilLean.Lang.Equiv.StfTopological (stf_converges)
open VerilLean.Lang.Equiv.Bridge (buildExecProcesses)
open VerilLean.Lang.Equiv.FullEquiv (module_concrete_equiv)
open VerilLean.Lang.Equiv.RegisterEquiv (SeqProcess evalSeqProcesses register_transition_equiv)
open VerilLean.Lib
open VerilLean.Lib.HMapLemmas

-- ============================================================================
-- End-to-end: Hierarchical STF = Standard Semantics
-- ============================================================================

/- For a module evaluated with any mtrss (including one built from a
   hierarchy), if the processes satisfy ModuleWellFormed and the STF
   converges while Standard reaches quiescence, the results are
   observably equivalent.

   This works for hierarchical evaluation because buildExecProcesses
   wraps module instances as ExecProcesses (calling mtrs.func), and
   the existing equivalence proof treats them as abstract processes.
   When the mtrss is built via hierarchy_all_faithful, each mtrs.func
   is faithful to its submodule's concrete semantics. -/
theorem hierarchy_stf_std_equiv
    (mod : module_decl) (decls : Decls) (funcs : Funcs)
    (mtrss : MTrss) (ctxs : State) (cpos : HPath)
    (ifw : IFW) (flops : Flops)
    (inputs flops_vids : List VId)
    (procs := buildExecProcesses decls funcs mtrss ctxs cpos ifw flops mod)
    (hwf : ModuleWellFormed procs inputs flops_vids)
    (s0 : State)
    (sf_stf_fields : List (String × HMap))
    (sf_std : State)
    (hstf : stf_converges procs s0 (.str sf_stf_fields))
    (hstd : StdReachesQuiet procs s0 sf_std)
    (hnodup : NoDupKeys sf_stf_fields)
    (hUpdStr : forall ep, ep ∈ procs -> forall u, ep.exec (.str sf_stf_fields) = .ok u ->
        exists u_fields, u = .str u_fields ∧
          forall k, k ∈ u_fields.map Prod.fst -> k ∈ sf_stf_fields.map Prod.fst) :
    StateEquiv (.str sf_stf_fields) sf_std :=
  module_concrete_equiv mod decls funcs mtrss ctxs cpos ifw flops
    inputs flops_vids (procs := procs) hwf s0 sf_stf_fields sf_std
    hstf hstd hnodup hUpdStr

/- Full register transition equivalence for hierarchical modules.
   Both combinational wire states and sequential flop updates agree. -/
theorem hierarchy_register_equiv
    (mod : module_decl) (decls : Decls) (funcs : Funcs)
    (mtrss : MTrss) (ctxs : State) (cpos : HPath)
    (ifw : IFW) (flops : Flops)
    (inputs flops_vids : List VId)
    (procs := buildExecProcesses decls funcs mtrss ctxs cpos ifw flops mod)
    (hwf : ModuleWellFormed procs inputs flops_vids)
    (s0 : State)
    (sf_stf_fields : List (String × HMap))
    (sf_std : State)
    (hstf : stf_converges procs s0 (.str sf_stf_fields))
    (hstd : StdReachesQuiet procs s0 sf_std)
    (hnodup : NoDupKeys sf_stf_fields)
    (hUpdStr : forall ep, ep ∈ procs -> forall u, ep.exec (.str sf_stf_fields) = .ok u ->
        exists u_fields, u = .str u_fields ∧
          forall k, k ∈ u_fields.map Prod.fst -> k ∈ sf_stf_fields.map Prod.fst)
    (seqProcs : List SeqProcess)
    (hSeqFrame : forall sp, sp ∈ seqProcs ->
        forall s1 s2 : State,
        (forall v, v ∈ sp.reads -> haccess s1 v = haccess s2 v) ->
        sp.exec s1 = sp.exec s2) :
    StateEquiv (.str sf_stf_fields) sf_std ∧
    evalSeqProcesses seqProcs (.str sf_stf_fields) = evalSeqProcesses seqProcs sf_std :=
  register_transition_equiv mod decls funcs mtrss ctxs cpos ifw flops inputs flops_vids
    (procs := procs) hwf s0 sf_stf_fields sf_std hstf hstd hnodup hUpdStr
    seqProcs hSeqFrame

end VerilLean.Lang.Equiv.FullHierarchyEquiv
