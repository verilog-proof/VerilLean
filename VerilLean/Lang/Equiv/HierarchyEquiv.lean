/- # Hierarchy Equivalence.
   Proves that hierarchical module evaluation is correct when submodule MTrs
   are faithful. Key technical contribution: frame lemma for mtrss agreement. -/

import VerilLean.Lib.Lib
import VerilLean.Lang.Syntax
import VerilLean.Lang.Semantics
import VerilLean.Lang.Equiv.MTrsFaithful
import VerilLean.Lang.Equiv.Flatten

namespace VerilLean.Lang.Equiv.HierarchyEquiv

open VerilLean.Lang.Syntax
open VerilLean.Lang.Semantics
open VerilLean.Lang.Equiv.MTrsFaithful
  (MTrsFaithful AllMTrsFaithful ModuleEnv ModuleEvalOk buildMTrs buildMTrs_faithful)
open VerilLean.Lib

-- ============================================================================
-- Part 1: Instanced module names
-- ============================================================================

def instancedModulesIns : module_ins -> List VId
  | .module mid _ _ => [mid]

def instancedModulesMOGI : module_or_generate_item -> List VId
  | .common _ => []
  | .ins mi => instancedModulesIns mi

mutual
def instancedModulesGMI : generate_module_item -> List VId
  | .module mogi => instancedModulesMOGI mogi
  | .cond _ t f =>
    instancedModulesGMI t ++
    (match f with | some g => instancedModulesGMI g | none => [])
  | .block gmis => instancedModulesGMIList gmis

def instancedModulesGMIList : List generate_module_item -> List VId
  | [] => []
  | g :: gs => instancedModulesGMI g ++ instancedModulesGMIList gs
end

def instancedModulesNPMI : non_port_module_item -> List VId
  | .generated_module_ins (.generated gmi) => instancedModulesGMI gmi
  | .module_or_generate_item mogi => instancedModulesMOGI mogi

def instancedModulesMI : module_item -> List VId
  | .port_decl _ => []
  | .non_port np => instancedModulesNPMI np

def instancedModulesMIs : module_items -> List VId
  | .one mi => instancedModulesMI mi
  | .cons mi mis => instancedModulesMI mi ++ instancedModulesMIs mis

def instancedModules : module_decl -> List VId
  | .ansi _ _ _ items => instancedModulesMIs items

-- ============================================================================
-- Part 2: Frame lemma — mtrss agreement
-- ============================================================================

-- Base case: trsVModuleInsMTrs only looks up mtrss(mid).
-- If two mtrss maps agree on mid, results are identical.
theorem trsVModuleInsMTrs_frame
    (decls : Decls) (funcs : Funcs) (mtrss1 mtrss2 : MTrss)
    (ctxs : State) (cpos : HPath) (ifw : IFW) (flops : Flops)
    (mid : VId) (pva : param_value_assigns) (iid : VId)
    (npcs : named_port_conns) (nw : NW)
    (hagree : mtrss1 mid = mtrss2 mid) :
    trsVModuleInsMTrs decls funcs mtrss1 ctxs cpos ifw flops
      (.module mid pva (.hier iid (.named npcs))) nw =
    trsVModuleInsMTrs decls funcs mtrss2 ctxs cpos ifw flops
      (.module mid pva (.hier iid (.named npcs))) nw := by
  simp only [trsVModuleInsMTrs, hagree]

-- trsVModuleIns is just trsVModuleInsMTrs
theorem trsVModuleIns_frame
    (decls : Decls) (funcs : Funcs) (mtrss1 mtrss2 : MTrss)
    (ctxs : State) (cpos : HPath) (ifw : IFW) (flops : Flops)
    (mi : module_ins) (nw : NW)
    (hagree : forall mid, mid ∈ instancedModulesIns mi -> mtrss1 mid = mtrss2 mid) :
    trsVModuleIns decls funcs mtrss1 ctxs cpos ifw flops mi nw =
    trsVModuleIns decls funcs mtrss2 ctxs cpos ifw flops mi nw := by
  cases mi with
  | module mid pva hi =>
    cases hi with
    | hier iid pcs =>
      cases pcs with
      | named npcs =>
        simp only [trsVModuleIns]
        apply trsVModuleInsMTrs_frame
        apply hagree
        simp [instancedModulesIns]

-- trsVModuleOrGenerateItem: .common doesn't use mtrss, .ins uses it
theorem trsVModuleOrGenerateItem_frame
    (decls : Decls) (funcs : Funcs) (mtrss1 mtrss2 : MTrss)
    (ctxs : State) (cpos : HPath) (ifw : IFW) (flops : Flops)
    (isComb : Bool) (mogi : module_or_generate_item) (nw : NW)
    (hagree : forall mid, mid ∈ instancedModulesMOGI mogi ->
      mtrss1 mid = mtrss2 mid) :
    trsVModuleOrGenerateItem decls funcs mtrss1 ctxs cpos ifw flops isComb mogi nw =
    trsVModuleOrGenerateItem decls funcs mtrss2 ctxs cpos ifw flops isComb mogi nw := by
  cases mogi with
  | common ci =>
    simp only [trsVModuleOrGenerateItem]
  | ins mi =>
    simp only [trsVModuleOrGenerateItem]
    exact trsVModuleIns_frame decls funcs mtrss1 mtrss2 ctxs cpos ifw flops mi nw
      (fun mid hmid => hagree mid (by simp [instancedModulesMOGI]; exact hmid))

-- Generate module items frame (mutual recursion with termination_by sizeOf)
mutual
theorem trsVGenerateModuleItem_frame
    (decls : Decls) (funcs : Funcs) (mtrss1 mtrss2 : MTrss)
    (ctxs : State) (cpos : HPath) (ifw : IFW) (flops : Flops)
    (isComb : Bool) (gmi : generate_module_item) (nw : NW)
    (hagree : forall mid, mid ∈ instancedModulesGMI gmi ->
      mtrss1 mid = mtrss2 mid) :
    trsVGenerateModuleItem decls funcs mtrss1 ctxs cpos ifw flops isComb gmi nw =
    trsVGenerateModuleItem decls funcs mtrss2 ctxs cpos ifw flops isComb gmi nw := by
  match gmi with
  | .module mogi =>
    simp only [trsVGenerateModuleItem.eq_1]
    exact trsVModuleOrGenerateItem_frame decls funcs mtrss1 mtrss2 ctxs cpos ifw flops
      isComb mogi nw (fun mid hmid => hagree mid
      (by simp [instancedModulesGMI]; exact hmid))
  | .cond ce tgmi none =>
    simp only [trsVGenerateModuleItem.eq_2]
    cases h : evalExpr decls funcs ctxs cpos ifw nw ce with
    | error e => rfl
    | ok cv =>
      simp only [bind, Except.bind]
      split
      · rfl
      · exact trsVGenerateModuleItem_frame decls funcs mtrss1 mtrss2 ctxs cpos ifw flops
          isComb tgmi nw (fun mid hmid => hagree mid
          (List.mem_append_left _ hmid))
  | .cond ce tgmi (some fgmi') =>
    simp only [trsVGenerateModuleItem.eq_3]
    cases h : evalExpr decls funcs ctxs cpos ifw nw ce with
    | error e => rfl
    | ok cv =>
      simp only [bind, Except.bind]
      split
      · exact trsVGenerateModuleItem_frame decls funcs mtrss1 mtrss2 ctxs cpos ifw flops
          isComb fgmi' nw (fun mid hmid => hagree mid
          (List.mem_append_right _ hmid))
      · exact trsVGenerateModuleItem_frame decls funcs mtrss1 mtrss2 ctxs cpos ifw flops
          isComb tgmi nw (fun mid hmid => hagree mid
          (List.mem_append_left _ hmid))
  | .block gmis =>
    simp only [trsVGenerateModuleItem.eq_4]
    exact trsVGenerateModuleItemList_frame decls funcs mtrss1 mtrss2 ctxs cpos ifw flops
      isComb gmis nw hagree
termination_by sizeOf gmi

theorem trsVGenerateModuleItemList_frame
    (decls : Decls) (funcs : Funcs) (mtrss1 mtrss2 : MTrss)
    (ctxs : State) (cpos : HPath) (ifw : IFW) (flops : Flops)
    (isComb : Bool) (gmis : List generate_module_item) (nw : NW)
    (hagree : forall mid, mid ∈ instancedModulesGMIList gmis ->
      mtrss1 mid = mtrss2 mid) :
    trsVGenerateModuleItemList decls funcs mtrss1 ctxs cpos ifw flops isComb gmis nw =
    trsVGenerateModuleItemList decls funcs mtrss2 ctxs cpos ifw flops isComb gmis nw := by
  match gmis with
  | [] => rfl
  | gmi :: rest =>
    simp only [trsVGenerateModuleItemList.eq_2]
    have hgmi := trsVGenerateModuleItem_frame decls funcs mtrss1 mtrss2 ctxs cpos ifw flops
      isComb gmi nw (fun mid hmid => hagree mid
      (List.mem_append_left _ hmid))
    rw [hgmi]
    cases trsVGenerateModuleItem decls funcs mtrss2 ctxs cpos ifw flops isComb gmi nw with
    | error e => rfl
    | ok val =>
      simp only [bind, Except.bind]
      have hrest := trsVGenerateModuleItemList_frame decls funcs mtrss1 mtrss2 ctxs cpos ifw flops
        isComb rest (hupds nw val.1) (fun mid hmid => hagree mid
        (List.mem_append_right _ hmid))
      rw [hrest]
termination_by sizeOf gmis
end

-- Non-port module item frame
theorem trsVNonPortModuleItem_frame
    (decls : Decls) (funcs : Funcs) (mtrss1 mtrss2 : MTrss)
    (ctxs : State) (cpos : HPath) (ifw : IFW) (flops : Flops)
    (isComb : Bool) (np : non_port_module_item) (nw : NW)
    (hagree : forall mid, mid ∈ instancedModulesNPMI np ->
      mtrss1 mid = mtrss2 mid) :
    trsVNonPortModuleItem decls funcs mtrss1 ctxs cpos ifw flops isComb np nw =
    trsVNonPortModuleItem decls funcs mtrss2 ctxs cpos ifw flops isComb np nw := by
  cases np with
  | generated_module_ins gmi =>
    cases gmi with
    | generated g =>
      simp only [trsVNonPortModuleItem]
      exact trsVGenerateModuleItem_frame decls funcs mtrss1 mtrss2 ctxs cpos ifw flops
        isComb g nw (fun mid hmid => hagree mid
        (by simp [instancedModulesNPMI]; exact hmid))
  | module_or_generate_item mogi =>
    simp only [trsVNonPortModuleItem]
    exact trsVModuleOrGenerateItem_frame decls funcs mtrss1 mtrss2 ctxs cpos ifw flops
      isComb mogi nw (fun mid hmid => hagree mid
      (by simp [instancedModulesNPMI]; exact hmid))

-- Module item frame
theorem trsVModuleItem_frame
    (decls : Decls) (funcs : Funcs) (mtrss1 mtrss2 : MTrss)
    (ctxs : State) (cpos : HPath) (ifw : IFW) (flops : Flops)
    (isComb : Bool) (mi : module_item) (nw : NW)
    (hagree : forall mid, mid ∈ instancedModulesMI mi ->
      mtrss1 mid = mtrss2 mid) :
    trsVModuleItem decls funcs mtrss1 ctxs cpos ifw flops isComb mi nw =
    trsVModuleItem decls funcs mtrss2 ctxs cpos ifw flops isComb mi nw := by
  cases mi with
  | port_decl _ => rfl
  | non_port np =>
    simp only [trsVModuleItem]
    exact trsVNonPortModuleItem_frame decls funcs mtrss1 mtrss2 ctxs cpos ifw flops
      isComb np nw (fun mid hmid => hagree mid
      (by simp [instancedModulesMI]; exact hmid))

-- Module items frame (induction on module_items)
theorem trsVModuleItems_frame
    (decls : Decls) (funcs : Funcs) (mtrss1 mtrss2 : MTrss)
    (ctxs : State) (cpos : HPath) (ifw : IFW) (flops : Flops)
    (isComb : Bool) (mis : module_items) (nw : NW)
    (hagree : forall mid, mid ∈ instancedModulesMIs mis ->
      mtrss1 mid = mtrss2 mid) :
    trsVModuleItems decls funcs mtrss1 ctxs cpos ifw flops isComb mis nw =
    trsVModuleItems decls funcs mtrss2 ctxs cpos ifw flops isComb mis nw := by
  induction mis generalizing nw with
  | one mi =>
    simp only [trsVModuleItems]
    exact trsVModuleItem_frame decls funcs mtrss1 mtrss2 ctxs cpos ifw flops isComb mi nw
      (fun mid hmid => hagree mid (by simp [instancedModulesMIs]; exact hmid))
  | cons mi rest ih =>
    simp only [trsVModuleItems]
    have hmi := trsVModuleItem_frame decls funcs mtrss1 mtrss2 ctxs cpos ifw flops isComb mi nw
      (fun mid hmid => hagree mid
      (by simp [instancedModulesMIs]; left; exact hmid))
    have hrest : forall nw', trsVModuleItems decls funcs mtrss1 ctxs cpos ifw flops isComb rest nw' =
        trsVModuleItems decls funcs mtrss2 ctxs cpos ifw flops isComb rest nw' :=
      fun nw' => ih nw' (fun mid hmid => hagree mid
        (by simp [instancedModulesMIs]; right; exact hmid))
    simp only [hmi, hrest]

-- Module declaration frame
theorem trsVModuleDecl_frame
    (mtrss1 mtrss2 : MTrss) (ctxs : State) (cpos : HPath)
    (decls : Decls) (funcs : Funcs) (mod : module_decl)
    (ifw : IFW) (flops : Flops)
    (hagree : forall mid, mid ∈ instancedModules mod -> mtrss1 mid = mtrss2 mid) :
    trsVModuleDecl mtrss1 ctxs cpos decls funcs mod ifw flops =
    trsVModuleDecl mtrss2 ctxs cpos decls funcs mod ifw flops := by
  cases mod with
  | ansi name pps ports mitems =>
    simp only [trsVModuleDecl]
    -- trsVParamPorts doesn't use mtrss, so nw0 is the same
    cases h : trsVParamPorts decls funcs ctxs cpos ifw HMap.empty pps with
    | error e => rfl
    | ok nw0 =>
      simp only [bind, Except.bind]
      exact trsVModuleItems_frame decls funcs mtrss1 mtrss2 ctxs cpos ifw flops true mitems nw0
        (fun mid hmid => hagree mid (by simp [instancedModules]; exact hmid))

-- Module declaration IFF frame
theorem trsVModuleDecl_IFF_frame
    (mtrss1 mtrss2 : MTrss) (ctxs : State) (cpos : HPath)
    (decls : Decls) (funcs : Funcs) (mod : module_decl)
    (iff_ : IFF)
    (hagree : forall mid, mid ∈ instancedModules mod -> mtrss1 mid = mtrss2 mid) :
    trsVModuleDecl_IFF mtrss1 ctxs cpos decls funcs mod iff_ =
    trsVModuleDecl_IFF mtrss2 ctxs cpos decls funcs mod iff_ := by
  simp only [trsVModuleDecl_IFF]
  rw [trsVModuleDecl_frame mtrss1 mtrss2 ctxs cpos decls funcs mod iff_.1 iff_.2 hagree]

-- Fixed-point iteration frame
theorem trsM_iff_rep_frame
    (mtrss1 mtrss2 : MTrss) (ctxs : State) (cpos : HPath)
    (decls : Decls) (funcs : Funcs) (mod : module_decl) (n : Nat) (iff_ : IFF)
    (hagree : forall mid, mid ∈ instancedModules mod -> mtrss1 mid = mtrss2 mid) :
    trsM_iff_rep mtrss1 ctxs cpos decls funcs mod n iff_ =
    trsM_iff_rep mtrss2 ctxs cpos decls funcs mod n iff_ := by
  induction n generalizing iff_ with
  | zero => rfl
  | succ n ih =>
    simp only [trsM_iff_rep]
    rw [trsVModuleDecl_IFF_frame mtrss1 mtrss2 ctxs cpos decls funcs mod iff_ hagree]
    cases trsVModuleDecl_IFF mtrss2 ctxs cpos decls funcs mod iff_ with
    | error e => rfl
    | ok iff' => exact ih iff'

-- trsM_IFF frame (5 iterations)
theorem trsM_IFF_frame
    (mtrss1 mtrss2 : MTrss) (ctxs : State) (cpos : HPath)
    (decls : Decls) (funcs : Funcs) (mod : module_decl) (iff_ : IFF)
    (hagree : forall mid, mid ∈ instancedModules mod -> mtrss1 mid = mtrss2 mid) :
    trsM_IFF mtrss1 ctxs cpos decls funcs mod iff_ =
    trsM_IFF mtrss2 ctxs cpos decls funcs mod iff_ := by
  simp only [trsM_IFF]
  exact trsM_iff_rep_frame mtrss1 mtrss2 ctxs cpos decls funcs mod 5 iff_ hagree

-- ============================================================================
-- Part 3: Module Hierarchy
-- ============================================================================

/- A module hierarchy: a set of modules with a topological ordering
   (leaves first, parents last). -/
structure ModuleHierarchy where
  env : ModuleEnv
  order : List VId
  orderComplete : forall mid, mid ∈ order -> (env.modDecl mid).isSome = true
  orderNoDup : order.Nodup
  /-- IO port vids for each module. -/
  ioVids : VId -> List VId × List VId
  /-- Topological ordering: submodules come before parents. -/
  topoOrder : forall (i j : Nat) (mi mj : VId),
    order[i]? = some mi -> order[j]? = some mj ->
    forall mod, env.modDecl mj = some mod ->
    mi ∈ instancedModules mod -> i < j

/- Build MTrs bottom-up for all modules in a hierarchy. -/
def buildMTrssFromHierarchy (hier : ModuleHierarchy) : MTrss :=
  hier.order.foldl (fun acc mid =>
    match hier.env.modDecl mid with
    | some mod =>
      let (ivids, ovids) := hier.ioVids mid
      let mtrs := buildMTrs mod (hier.env.modDecls mid) (hier.env.modFuncs mid)
        acc (hier.env.modCtxs mid) (hier.env.modCpos mid) ivids ovids
      fmapMerge (fmapSingle mid mtrs) acc
    | none => acc) fmapEmpty

-- ============================================================================
-- Part 4: Inductive Faithfulness
-- ============================================================================

/- Key property: fmapMerge preserves existing entries for different keys. -/
private theorem fmapMerge_single_other {A : Type} (k1 k2 : VId) (v : A)
    (acc : TrsFMap A) (hne : k1 ≠ k2) :
    fmapMerge (fmapSingle k1 v) acc k2 = acc k2 := by
  simp only [fmapMerge, fmapSingle]
  have : (k1 == k2) = false := by
    simp [BEq.beq]
    exact hne
  simp [this]

/- After processing the hierarchy, all modules that were processed have
   their MTrs in the result. The MTrs for each module was built with
   buildMTrs using the accumulated mtrss at that point.

   We prove AllMTrsFaithful by showing that for each module M:
   1. M's MTrs was built via buildMTrs with some intermediate acc_M
   2. buildMTrs_faithful gives faithfulness relative to acc_M
   3. The frame lemma shows faithfulness lifts to the final mtrss
      (since acc_M and final mtrss agree on M's submodules) -/

/- All modules evaluate successfully with the built mtrss. -/
def HierarchyEvalOk (hier : ModuleHierarchy) (mtrss : MTrss) : Prop :=
  forall mid mod, mid ∈ hier.order ->
    hier.env.modDecl mid = some mod ->
    ModuleEvalOk mod (hier.env.modDecls mid) (hier.env.modFuncs mid)
      mtrss (hier.env.modCtxs mid) (hier.env.modCpos mid)

/- Key theorem: if mtrss is a "fixpoint" — every entry was built via
   buildMTrs with this same mtrss — then AllMTrsFaithful holds.
   This is the conceptually clean version; the connection to
   buildMTrssFromHierarchy (which uses intermediate acc) requires the
   frame lemma and is handled separately. -/
theorem allFaithful_of_fixpoint
    (mtrss : MTrss) (env : ModuleEnv)
    (hBuild : forall mid mtrs mod,
      mtrss mid = .ok mtrs -> env.modDecl mid = some mod ->
      exists ivids ovids,
        mtrs = buildMTrs mod (env.modDecls mid) (env.modFuncs mid) mtrss
          (env.modCtxs mid) (env.modCpos mid) ivids ovids)
    (hEvalOk : forall mid mod,
      env.modDecl mid = some mod ->
      ModuleEvalOk mod (env.modDecls mid) (env.modFuncs mid) mtrss
        (env.modCtxs mid) (env.modCpos mid)) :
    AllMTrsFaithful mtrss env := by
  intro mid mtrs mod hmtrss hmod
  obtain ⟨ivids, ovids, hbuild⟩ := hBuild mid mtrs mod hmtrss hmod
  rw [hbuild]
  apply buildMTrs_faithful
  exact hEvalOk mid mod hmod

-- ## Main theorem (via auxiliary)

/- Auxiliary: AllMTrsFaithful follows from two properties of the mtrss map:
   1. Each entry was built via buildMTrs with some intermediate acc
   2. That intermediate acc agrees with the final mtrss on instanced modules

   This cleanly separates the fold bookkeeping from the faithfulness proof. -/
theorem hierarchy_all_faithful_aux
    (mtrss : MTrss) (env : ModuleEnv)
    (hEvalOk : forall mid mod,
      env.modDecl mid = some mod ->
      ModuleEvalOk mod (env.modDecls mid) (env.modFuncs mid) mtrss
        (env.modCtxs mid) (env.modCpos mid))
    -- Each entry was built with some intermediate acc that agrees with mtrss on submodules
    (hFold : forall mid mtrs mod,
      mtrss mid = .ok mtrs -> env.modDecl mid = some mod ->
      exists acc_mid,
        mtrs.func = (buildMTrs mod (env.modDecls mid) (env.modFuncs mid)
          acc_mid (env.modCtxs mid) (env.modCpos mid)
          mtrs.inputVids mtrs.outputVids).func ∧
        forall sub, sub ∈ instancedModules mod -> acc_mid sub = mtrss sub) :
    AllMTrsFaithful mtrss env := by
  intro mid mtrs mod hmtrss hmod
  obtain ⟨acc_mid, hfunc_eq, hagree⟩ := hFold mid mtrs mod hmtrss hmod
  intro inputState flopState
  -- Goal: trsM_IFF mtrss ... = .ok (mtrs.func inputState flopState)
  -- By frame: trsM_IFF mtrss ... = trsM_IFF acc_mid ...
  rw [trsM_IFF_frame mtrss acc_mid (env.modCtxs mid) (env.modCpos mid)
    (env.modDecls mid) (env.modFuncs mid) mod
    (hupds flopState inputState, flopState)
    (fun sub hsub => (hagree sub hsub).symm)]
  -- Goal: trsM_IFF acc_mid ... = .ok (mtrs.func inputState flopState)
  -- mtrs.func = (buildMTrs ... acc_mid ...).func
  rw [hfunc_eq]
  -- Goal: trsM_IFF acc_mid ... = .ok ((buildMTrs ... acc_mid ...).func inp fl)
  -- This is buildMTrs_faithful applied to acc_mid
  -- Need ModuleEvalOk ... acc_mid ... (from ModuleEvalOk ... mtrss ... + frame)
  have hEvalOk_acc : ModuleEvalOk mod (env.modDecls mid) (env.modFuncs mid)
      acc_mid (env.modCtxs mid) (env.modCpos mid) := by
    intro ifw' fl'
    have ⟨ifw'', fl'', hok⟩ := hEvalOk mid mod hmod ifw' fl'
    rw [trsM_IFF_frame mtrss acc_mid (env.modCtxs mid) (env.modCpos mid)
      (env.modDecls mid) (env.modFuncs mid) mod (ifw', fl')
      (fun sub hsub => (hagree sub hsub).symm)] at hok
    exact ⟨ifw'', fl'', hok⟩
  exact buildMTrs_faithful mod (env.modDecls mid) (env.modFuncs mid)
    acc_mid (env.modCtxs mid) (env.modCpos mid) mtrs.inputVids mtrs.outputVids
    hEvalOk_acc inputState flopState

/- Main theorem: buildMTrssFromHierarchy produces faithful MTrs.
   Uses hierarchy_all_faithful_aux with the fold properties. -/
/- The fold properties required by hierarchy_all_faithful_aux.
   These are established by the caller who constructs the hierarchy and
   knows the fold structure. Separating them makes the proof modular. -/
structure HierarchyFoldProps (mtrss : MTrss) (env : ModuleEnv) where
  evalOk : forall mid mod,
    env.modDecl mid = some mod ->
    ModuleEvalOk mod (env.modDecls mid) (env.modFuncs mid) mtrss
      (env.modCtxs mid) (env.modCpos mid)
  foldProp : forall mid mtrs mod,
    mtrss mid = .ok mtrs -> env.modDecl mid = some mod ->
    exists acc_mid,
      mtrs.func = (buildMTrs mod (env.modDecls mid) (env.modFuncs mid)
        acc_mid (env.modCtxs mid) (env.modCpos mid)
        mtrs.inputVids mtrs.outputVids).func ∧
      forall sub, sub ∈ instancedModules mod -> acc_mid sub = mtrss sub

/- Main theorem: any mtrss satisfying the fold properties is faithful. -/
theorem hierarchy_all_faithful
    (mtrss : MTrss) (env : ModuleEnv)
    (props : HierarchyFoldProps mtrss env) :
    AllMTrsFaithful mtrss env :=
  hierarchy_all_faithful_aux mtrss env props.evalOk props.foldProp

end VerilLean.Lang.Equiv.HierarchyEquiv
