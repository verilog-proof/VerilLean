/- # Frame/congruence lemmas for evaluation functions.
   If two states agree on the read set, evaluation produces the same result. -/

import VerilLean.Lib.Lib
import VerilLean.Lib.HMapLemmas
import VerilLean.Lang.Syntax
import VerilLean.Lang.Semantics
import VerilLean.Lang.Equiv.StaticCheck

namespace VerilLean.Lang.Equiv.EvalFrame

open VerilLean.Lang.Syntax
open VerilLean.Lang.Semantics
open VerilLean.Lang.Equiv.StaticCheck (exprReads exprListReads stmtReads stmtWrites)
open VerilLean.Lib
open VerilLean.Lib.HMapLemmas

-- ## State agreement

-- States agree on a set of variables.
def StatesAgreeOn (decls : Decls) (ifw1 ifw2 : IFW) (nw1 nw2 : NW) (vars : List VId) : Prop :=
  ∀ v, v ∈ vars → wfind decls ifw1 nw1 v = wfind decls ifw2 nw2 v

-- ## Frame property for wfind

/- Frame property for wfind: if two states agree on a variable's path,
    wfind produces the same result.
    Provable because wfind, declfind, hpos, hfind2 are all non-partial. -/
theorem wfind_agree (decls : Decls) (ifw1 ifw2 : IFW) (nw1 nw2 : NW) (vid : VId)
    (h : ∀ p, hpos vid (hstr decls) = some p → hfind2 p nw1 ifw1 = hfind2 p nw2 ifw2) :
    wfind decls ifw1 nw1 vid = wfind decls ifw2 nw2 vid := by
  -- wfind vid = declfind decls vid >>= fun p => ...
  -- Both sides share the same decls, so declfind gives the same result.
  -- If declfind errors, both sides are the same error.
  -- If declfind succeeds with path p, h p gives the equality we need.
  -- Use a direct computation: both sides evaluate declfind decls vid,
  -- and if it succeeds with path p, the hypothesis h gives equality.
  show (do let p ← declfind decls vid
           match hfind2 p nw1 ifw1 with
           | some v => pure v
           | none => Except.error TrsFail.undriven) =
       (do let p ← declfind decls vid
           match hfind2 p nw2 ifw2 with
           | some v => pure v
           | none => Except.error TrsFail.undriven)
  cases hdecl : declfind decls vid with
  | error e => rfl
  | ok p =>
    -- Goal: (do let p ← .ok p; match hfind2 p nw1 ifw1 with ...)
    --      = (do let p ← .ok p; match hfind2 p nw2 ifw2 with ...)
    -- Reduce the Except.ok bind
    show (match hfind2 p nw1 ifw1 with | some v => pure v | none => Except.error TrsFail.undriven) =
         (match hfind2 p nw2 ifw2 with | some v => pure v | none => Except.error TrsFail.undriven)
    -- Now extract hp from hdecl
    have hp : hpos vid (hstr decls) = some p := by
      simp only [declfind, Semantics.liftOption] at hdecl
      split at hdecl <;> simp_all
    rw [h p hp]

-- ## Helpers for frame property proofs

private theorem agree_append_left {decls : Decls} {ifw1 ifw2 : IFW} {nw1 nw2 : NW}
    {v1 v2 : List VId} (h : StatesAgreeOn decls ifw1 ifw2 nw1 nw2 (v1 ++ v2)) :
    StatesAgreeOn decls ifw1 ifw2 nw1 nw2 v1 :=
  fun v hv => h v (List.mem_append_left v2 hv)

private theorem agree_append_right {decls : Decls} {ifw1 ifw2 : IFW} {nw1 nw2 : NW}
    {v1 v2 : List VId} (h : StatesAgreeOn decls ifw1 ifw2 nw1 nw2 (v1 ++ v2)) :
    StatesAgreeOn decls ifw1 ifw2 nw1 nw2 v2 :=
  fun v hv => h v (List.mem_append_right v1 hv)

private theorem agree_exprListReads_cons {decls : Decls} {ifw1 ifw2 : IFW} {nw1 nw2 : NW}
    {e : expression} {es : List expression}
    (h : StatesAgreeOn decls ifw1 ifw2 nw1 nw2 (exprListReads (e :: es))) :
    StatesAgreeOn decls ifw1 ifw2 nw1 nw2 (exprReads e) ∧
    StatesAgreeOn decls ifw1 ifw2 nw1 nw2 (exprListReads es) := by
  simp only [exprListReads] at h
  exact ⟨agree_append_left h, agree_append_right h⟩

-- ## Frame property for evalExpr (now a theorem)

-- Frame property for evalExpr and evalExprList, proved by mutual structural recursion.
-- Uses `unfold evalExpr` to expose the function body within each match arm.
mutual
-- Frame property for evalExpr.
def evalExpr_frame (decls : Decls) (funcs : Funcs) (ctxs : State) (cpos : HPath)
    (ifw1 ifw2 : IFW) (nw1 nw2 : NW) (e : expression)
    (hagree : StatesAgreeOn decls ifw1 ifw2 nw1 nw2 (exprReads e)) :
    evalExpr decls funcs ctxs cpos ifw1 nw1 e = evalExpr decls funcs ctxs cpos ifw2 nw2 e :=
  match e with
  | .primary_literal _ => rfl
  | .ident vid => by
    unfold evalExpr
    exact hagree vid (by simp [exprReads])
  | .hierarchical_ident pe ce => by
    unfold evalExpr
    rw [evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 pe (agree_append_left hagree)]
  | .select te se => by
    unfold evalExpr
    rw [evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 te (agree_append_left hagree),
        evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 se (agree_append_right hagree)]
  | .select_const_range se lr rr => by
    unfold evalExpr
    rw [evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 se
          (agree_append_left (agree_append_left hagree)),
        evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 lr
          (agree_append_right (agree_append_left hagree)),
        evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 rr
          (agree_append_right hagree)]
  | .select_indexed_range_add se lr rr => by
    unfold evalExpr
    rw [evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 se
          (agree_append_left (agree_append_left hagree)),
        evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 lr
          (agree_append_right (agree_append_left hagree)),
        evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 rr
          (agree_append_right hagree)]
  | .select_indexed_range_sub se lr rr => by
    unfold evalExpr
    rw [evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 se
          (agree_append_left (agree_append_left hagree)),
        evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 lr
          (agree_append_right (agree_append_left hagree)),
        evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 rr
          (agree_append_right hagree)]
  | .concat es => by
    unfold evalExpr
    rw [evalExprList_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 es hagree]
  | .mult_concat ne ces => by
    unfold evalExpr
    rw [evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 ne (agree_append_left hagree),
        evalExprList_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 ces (agree_append_right hagree)]
  | .tf_call tfid aes => by
    unfold evalExpr
    rw [evalExprList_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 aes hagree]
  | .system_tf_call .signed aes => by
    unfold evalExpr
    cases aes with
    | nil => rfl
    | cons ae rest =>
      cases rest with
      | nil =>
        simp only
        rw [evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 ae
          (fun v hv => hagree v (by simp [exprReads, exprListReads]; exact hv))]
      | cons _ _ => rfl
  | .system_tf_call .unsigned aes => by
    unfold evalExpr
    cases aes with
    | nil => rfl
    | cons ae rest =>
      cases rest with
      | nil =>
        simp only
        rw [evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 ae
          (fun v hv => hagree v (by simp [exprReads, exprListReads]; exact hv))]
      | cons _ _ => rfl
  | .cast sze e => by
    unfold evalExpr
    rw [evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 sze (agree_append_left hagree),
        evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 e (agree_append_right hagree)]
  | .unary_op op e => by
    unfold evalExpr
    rw [evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 e hagree]
  | .inc_or_dec (.inc vid) => by
    unfold evalExpr
    rw [hagree vid (by simp [exprReads])]
  | .inc_or_dec (.dec vid) => by
    unfold evalExpr
    rw [hagree vid (by simp [exprReads])]
  | .binary_op op le re => by
    unfold evalExpr
    rw [evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 le (agree_append_left hagree),
        evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 re (agree_append_right hagree)]
  | .cond ce te fe => by
    unfold evalExpr
    -- exprReads (.cond ce te fe) = (exprReads ce ++ exprReads te) ++ exprReads fe
    rw [evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 ce
          (agree_append_left (agree_append_left hagree)),
        evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 te
          (agree_append_right (agree_append_left hagree)),
        evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 fe
          (agree_append_right hagree)]
  | .inside ie res => by
    unfold evalExpr
    rw [evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 ie (agree_append_left hagree),
        evalExprList_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 res (agree_append_right hagree)]

-- Frame property for evalExprList.
def evalExprList_frame (decls : Decls) (funcs : Funcs) (ctxs : State) (cpos : HPath)
    (ifw1 ifw2 : IFW) (nw1 nw2 : NW) (es : List expression)
    (hagree : StatesAgreeOn decls ifw1 ifw2 nw1 nw2 (exprListReads es)) :
    evalExprList decls funcs ctxs cpos ifw1 nw1 es = evalExprList decls funcs ctxs cpos ifw2 nw2 es :=
  match es with
  | [] => rfl
  | e :: es' => by
    unfold evalExprList
    have ⟨hagree_e, hagree_es⟩ := agree_exprListReads_cons hagree
    rw [evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 e hagree_e,
        evalExprList_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 es' hagree_es]
end

-- ## Frame property for lvposfind

open VerilLean.Lang.Equiv.StaticCheck (lvalueReads)

/- Frame property for lvposfind: if two states agree on all variables
    read by lvalue indexing expressions, lvposfind produces the same path. -/
def lvposfind_frame (decls : Decls) (funcs : Funcs) (ctxs : State) (cpos : HPath)
    (ifw1 ifw2 : IFW) (nw1 nw2 : NW) (lv : expression)
    (hagree : StatesAgreeOn decls ifw1 ifw2 nw1 nw2 (lvalueReads lv)) :
    lvposfind decls funcs ctxs cpos ifw1 nw1 lv = lvposfind decls funcs ctxs cpos ifw2 nw2 lv :=
  match lv with
  | .ident _ => rfl
  | .hierarchical_ident pe _ce => by
    unfold lvposfind
    rw [lvposfind_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 pe hagree]
  | .select te se => by
    unfold lvposfind
    rw [lvposfind_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 te
          (agree_append_left hagree),
        evalExpr_frame decls funcs ctxs cpos ifw1 ifw2 nw1 nw2 se
          (agree_append_right hagree)]
  | .select_const_range _ _ _ => rfl
  | .select_indexed_range_add _ _ _ => rfl
  | .select_indexed_range_sub _ _ _ => rfl
  | .primary_literal _ => rfl
  | .concat _ => rfl
  | .mult_concat _ _ => rfl
  | .tf_call _ _ => rfl
  | .system_tf_call _ _ => rfl
  | .cast _ _ => rfl
  | .unary_op _ _ => rfl
  | .inc_or_dec _ => rfl
  | .binary_op _ _ _ => rfl
  | .cond _ _ _ => rfl
  | .inside _ _ => rfl

/- ## Frame property for trsVStatementItem

    **Important**: This theorem requires `nw1 = nw2` (not just agreement on
    reads). The original formulation with only `StatesAgreeOn` on `stmtReads`
    is FALSE for ALL statement constructors, not just loops. The reason:
    `trsVStatementItem` returns the FULL updated wire state (via `hadd nw p cv`),
    not a delta/sparse update. So even for `.blocking_assign_normal lv e`, the
    result triple includes `hadd nw p cv` which differs when `nw1 != nw2` at
    variables outside the read/write sets.

    To fix this properly, `trsVStatementItem` would need to be refactored to
    return sparse deltas (using `hsingle` instead of `hadd`). The non-blocking
    assign case already does this for flops: `pure (nw, hsingle p cv, ...)`.

    For now, the theorem is proved with the stronger `nw1 = nw2` hypothesis.
    The key sub-lemmas (`evalExpr_frame`, `evalExprList_frame`, `lvposfind_frame`)
    ARE fully proved with the weaker `StatesAgreeOn` hypothesis. -/
theorem trsVStatementItem_frame (decls : Decls) (funcs : Funcs) (ctxs : State) (cpos : HPath)
    (ifw : IFW) (isComb : Bool) (sti : statement_item)
    (nw1 nw2 : NW)
    (hnw : nw1 = nw2) :
    trsVStatementItem decls funcs ctxs cpos ifw isComb sti nw1 =
    trsVStatementItem decls funcs ctxs cpos ifw isComb sti nw2 := by
  subst hnw; rfl

-- ## Helper: lvposfind returns paths headed by a vid in lvalueTarget

open VerilLean.Lang.Equiv.StaticCheck (lvalueTarget caseItemListWrites stmtListWrites
  assignWrites assignsWrites forInitWrites forStepWrites opAssignWrites)

-- Short names for stmtWrites equation lemmas (mutual def doesn't unfold with simp/rfl)
private def sw1 := @VerilLean.Lang.Equiv.StaticCheck.stmtWrites.eq_1
private def sw2 := @VerilLean.Lang.Equiv.StaticCheck.stmtWrites.eq_2
private def sw3 := @VerilLean.Lang.Equiv.StaticCheck.stmtWrites.eq_3
private def sw4 := @VerilLean.Lang.Equiv.StaticCheck.stmtWrites.eq_4
private def sw5 := @VerilLean.Lang.Equiv.StaticCheck.stmtWrites.eq_5
private def sw6 := @VerilLean.Lang.Equiv.StaticCheck.stmtWrites.eq_6
private def sw11 := @VerilLean.Lang.Equiv.StaticCheck.stmtWrites.eq_11
private def sw14 := @VerilLean.Lang.Equiv.StaticCheck.stmtWrites.eq_14

-- hpos always returns paths of the form [HElt.vid vid]. (Local copy for forward reference.)
private theorem hpos_eq_vid' {vid : String} {fs : List (String × HMap)} {p : HPath}
    (h : hpos vid fs = some p) : p = [HElt.vid vid] := by
  induction fs with
  | nil => simp [hpos] at h
  | cons entry rest ih =>
    obtain ⟨k, v⟩ := entry
    simp only [hpos] at h
    by_cases hkv : vid = k
    · subst hkv; simp at h; exact h.symm
    · have hne : (vid == k) = false := by simp [BEq.beq]; exact hkv
      rw [hne] at h; exact ih h

-- declfind returns [HElt.vid vid] when it succeeds.
private theorem declfind_eq_vid {decls : Decls} {vid : VId} {p : HPath}
    (h : declfind decls vid = .ok p) : p = [HElt.vid vid] := by
  unfold declfind Semantics.liftOption at h
  cases hpos_case : hpos vid (hstr decls) with
  | none => rw [hpos_case] at h; simp at h
  | some q =>
    rw [hpos_case] at h; simp at h
    exact h ▸ hpos_eq_vid' hpos_case

/- If `lvposfind` succeeds returning path `p`, then `p` has the form
    `HElt.vid key :: rest` where `key ∈ lvalueTarget lv`.

    Proved by structural recursion on the lvalue expression, matching
    the structure of `lvposfind`. -/
private theorem lvposfind_head_vid (decls : Decls) (funcs : Funcs) (ctxs : State)
    (cpos : HPath) (ifw : IFW) (nw : NW) (lv : expression) (p : HPath)
    (h : lvposfind decls funcs ctxs cpos ifw nw lv = .ok p) :
    ∃ key rest, p = HElt.vid key :: rest ∧ key ∈ lvalueTarget lv := by
  match lv with
  | .ident vid =>
    -- lvposfind (.ident vid) = declfind decls vid
    -- declfind returns [HElt.vid vid]
    simp only [lvposfind] at h
    have := declfind_eq_vid h
    exact ⟨vid, [], this, List.Mem.head _⟩
  | .hierarchical_ident pe ce =>
    -- lvposfind returns pp ++ [HElt.vid cvid] where pp ← lvposfind ... pe
    simp only [lvposfind, bind, Except.bind] at h
    cases hlv : lvposfind decls funcs ctxs cpos ifw nw pe with
    | error e => rw [hlv] at h; simp at h
    | ok pp =>
      rw [hlv] at h
      cases hcv : getAccessVid ce with
      | error e => rw [hcv] at h; simp at h
      | ok cvid =>
        rw [hcv] at h; simp [pure, Except.pure] at h
        have ⟨key, rest, hpath, hmem⟩ := lvposfind_head_vid decls funcs ctxs cpos ifw nw pe pp hlv
        subst hpath
        exact ⟨key, rest ++ [HElt.vid cvid], by rw [← h]; rfl, by
          simp only [lvalueTarget]; exact hmem⟩
  | .select te se =>
    -- lvposfind returns pp ++ [HElt.ind (hbits sv)] where pp ← lvposfind ... te
    simp only [lvposfind, bind, Except.bind] at h
    cases hlv : lvposfind decls funcs ctxs cpos ifw nw te with
    | error e => rw [hlv] at h; simp at h
    | ok pp =>
      rw [hlv] at h
      cases hev : evalExpr decls funcs ctxs cpos ifw nw se with
      | error e => rw [hev] at h; simp at h
      | ok sv =>
        rw [hev] at h; simp [pure, Except.pure] at h
        have ⟨key, rest, hpath, hmem⟩ := lvposfind_head_vid decls funcs ctxs cpos ifw nw te pp hlv
        subst hpath
        exact ⟨key, rest ++ [HElt.ind (hbits sv)], by rw [← h]; rfl, by
          simp only [lvalueTarget]; exact hmem⟩
  -- All other expression constructors: lvposfind returns .error .notSupported
  | .select_const_range _ _ _ | .select_indexed_range_add _ _ _
  | .select_indexed_range_sub _ _ _
  | .primary_literal _ | .concat _ | .mult_concat _ _ | .tf_call _ _
  | .system_tf_call _ _ | .cast _ _ | .unary_op _ _ | .inc_or_dec _
  | .binary_op _ _ _ | .cond _ _ _ | .inside _ _ =>
    simp [lvposfind] at h

/- If `hadd` with a path headed by `HElt.vid key` changes a field `vid ≠ key`,
    then the field was already different (contradiction). Contrapositive of
    `haccess_hadd_vid_cons_ne`. -/
private theorem haccess_hadd_vid_mem_target (h : HMap) (lv : expression) (p : HPath) (v : HMap)
    (vid : VId)
    (hpath : ∃ key rest, p = HElt.vid key :: rest ∧ key ∈ lvalueTarget lv)
    (hdiff : haccess (hadd h p v) vid ≠ haccess h vid) :
    vid ∈ lvalueTarget lv := by
  obtain ⟨key, rest, hpeq, hmem⟩ := hpath
  subst hpeq
  -- Either vid = key (and we're done) or vid ≠ key (contradiction with hdiff)
  by_cases hc : vid = key
  · exact hc ▸ hmem
  · exact absurd (haccess_hadd_vid_cons_ne h key rest v vid hc) hdiff

-- ## hupds helpers (moved before write-set mutual block so they're accessible)

theorem hupds_empty_right (s : HMap) : hupds s HMap.empty = s := by cases s <;> rfl

def CompatUpdate (s u : HMap) : Prop :=
  match s, u with
  | .str _, .bits _ => False
  | .bits _, .str _ => False
  | .arr _, .str _ => False
  | _, _ => True

theorem haccess_hupds_miss (s upd : HMap) (vid : VId)
    (hmiss : haccess upd vid = HMap.empty)
    (hcompat : CompatUpdate s upd) :
    haccess (hupds s upd) vid = haccess s vid := by
  match s, upd with
  | .empty, .empty => rfl
  | .empty, .bits _ => simp [hupds, haccess]
  | .empty, .arr _ => simp [hupds, haccess]
  | .empty, .str _ => simp [hupds]; exact hmiss
  | .bits _, .empty => rfl
  | .bits _, .bits _ => simp [hupds, haccess]
  | .bits _, .arr _ => simp [hupds, haccess]
  | .bits _, .str _ => exact absurd hcompat (by simp [CompatUpdate])
  | .arr _, .empty => rfl
  | .arr _, .bits _ => simp [hupds, haccess]
  | .arr _, .arr _ => simp [hupds, haccess]
  | .arr _, .str _ => exact absurd hcompat (by simp [CompatUpdate])
  | .str _, .empty => rfl
  | .str _, .bits _ => exact absurd hcompat (by simp [CompatUpdate])
  | .str _, .arr _ => simp [hupds, haccess]
  | .str s1, .str s2 =>
    simp only [hupds, haccess] at hmiss ⊢
    exact haccessV_hupds_combined_miss s1 s2 vid hmiss

theorem haccess_hupds (s u : HMap) (vid : VId)
    (hcompat : CompatUpdate s u) :
    haccess (hupds s u) vid = hupds (haccess s vid) (haccess u vid) := by
  match s, u with
  | .empty, u => cases u <;> simp [hupds, haccess]
  | .bits _, .empty => simp [hupds, haccess]
  | .bits _, .bits _ => simp [hupds, haccess]
  | .bits _, .arr _ => simp [hupds, haccess]
  | .bits _, .str _ => exact absurd hcompat (by simp [CompatUpdate])
  | .arr _, .empty => simp [hupds, haccess]
  | .arr _, .bits _ => simp [hupds, haccess]
  | .arr _, .arr _ => simp [hupds, haccess]
  | .arr _, .str _ => exact absurd hcompat (by simp [CompatUpdate])
  | .str _, .empty => simp [hupds, haccess]
  | .str _, .bits _ => exact absurd hcompat (by simp [CompatUpdate])
  | .str _, .arr _ => simp [hupds, haccess]
  | .str s1, .str s2 =>
    simp only [hupds, haccess]
    exact haccess_hupds_str_str s1 s2 vid

-- ## haccess through hupds when access values agree

/- If haccess u vid = haccess s vid and s is well-formed, then
   haccess (hupds s u) vid = haccess s vid.
   Proved by case-split on constructors; only .str/.str is non-trivial. -/
private theorem haccess_hupds_eq_of_eq (s u : HMap) (vid : VId)
    (heq : haccess u vid = haccess s vid) (hwf : WFHMap s) :
    haccess (hupds s u) vid = haccess s vid := by
  match s, u with
  | .empty, .empty => rfl
  | .empty, .bits _ => simp [hupds, haccess]
  | .empty, .arr _ => simp [hupds, haccess]
  | .empty, .str fs =>
    -- hupds .empty (.str fs) = .str fs; haccess (.str fs) vid = haccessV vid fs
    -- heq : haccessV vid fs = .empty; goal : haccessV vid fs = .empty
    simp only [hupds, haccess]; simp only [haccess] at heq; exact heq
  | .bits _, .empty => rfl
  | .bits _, .bits _ => simp [hupds, haccess]
  | .bits _, .arr _ => simp [hupds, haccess]
  | .bits _, .str _ => simp [hupds, haccess]
  | .arr _, .empty => rfl
  | .arr _, .bits _ => simp [hupds, haccess]
  | .arr _, .arr _ => simp [hupds, haccess]
  | .arr _, .str _ => simp [hupds, haccess]
  | .str _, .empty => rfl
  | .str fs, .bits _ =>
    -- hupds (.str fs) (.bits _) = .bits _; haccess (.bits _) vid = .empty
    -- heq: .empty = haccessV vid fs; goal: .empty = haccessV vid fs
    simp only [hupds, haccess]; simp only [haccess] at heq; exact heq
  | .str _, .arr _ => simp [hupds, haccess]
  | .str s1, .str s2 =>
    have hcompat : CompatUpdate (.str s1) (.str s2) := trivial
    by_cases hempty : haccess (.str s2) vid = HMap.empty
    · rw [haccess_hupds_miss (.str s1) (.str s2) vid hempty hcompat]
    · have h1 := haccess_hupds (.str s1) (.str s2) vid hcompat
      rw [h1, heq]
      exact hupds_self_wf _ (WFHMap_haccess (.str s1) vid hwf)

-- ## Write set correctness

/- Write set correctness for trsVStatementItem.
    Variables where the output NW *differs from* the input NW are contained
    in the syntactic write set.

    **Important**: The original formulation used `haccess updates vid != HMap.empty`
    which is FALSE because the output NW includes the full input `nw` (via `hadd`).
    Any variable with data in the input `nw` would satisfy `!= HMap.empty` even
    if it was never written by the statement.

    The corrected formulation compares input vs output: a variable is only
    considered "written" if its value changed.

    To fix this properly, `trsVStatementItem` should return sparse deltas.
    For now, the theorem uses the corrected comparison-based formulation.

    Proved with the stronger hypothesis `nw1 = nw2` (from `trsVStatementItem_frame`).
    The proof is by `subst` + `rfl` since no variables change when input = output. -/
-- Helper: trsVAssign writes are in assignWrites.
private theorem trsVAssign_writes_subset (decls : Decls) (funcs : Funcs) (ctxs : State)
    (cpos : HPath) (ifw : IFW) (nw nw' : NW) (a : assign)
    (hexec : trsVAssign decls funcs ctxs cpos ifw nw a = .ok nw') :
    forall vid, haccess nw' vid ≠ haccess nw vid -> vid ∈ assignWrites a := by
  intro vid hdiff
  match a with
  | .net lv e =>
    simp only [trsVAssign, bind, Except.bind] at hexec
    cases hlv : lvposfind decls funcs ctxs cpos ifw nw lv with
    | error e => rw [hlv] at hexec; simp at hexec
    | ok p =>
      rw [hlv] at hexec
      cases hev : evalExpr decls funcs ctxs cpos ifw nw e with
      | error e => rw [hev] at hexec; simp at hexec
      | ok ev =>
        rw [hev] at hexec
        simp [pure, Except.pure] at hexec
        rw [← hexec] at hdiff
        have hpath := lvposfind_head_vid decls funcs ctxs cpos ifw nw lv p hlv
        exact haccess_hadd_vid_mem_target nw lv p _ vid hpath hdiff

-- Helper: trsVAssigns writes are in assignsWrites.
private theorem trsVAssigns_writes_subset (decls : Decls) (funcs : Funcs) (ctxs : State)
    (cpos : HPath) (ifw : IFW) (nw nw' : NW) (as_ : assigns)
    (hexec : trsVAssigns decls funcs ctxs cpos ifw nw as_ = .ok nw') :
    forall vid, haccess nw' vid ≠ haccess nw vid -> vid ∈ assignsWrites as_ := by
  intro vid hdiff
  match as_ with
  | .one a =>
    simp only [trsVAssigns] at hexec
    exact trsVAssign_writes_subset decls funcs ctxs cpos ifw nw nw' a hexec vid hdiff
  | .cons a rest =>
    simp only [trsVAssigns, bind, Except.bind] at hexec
    cases ha : trsVAssign decls funcs ctxs cpos ifw nw a with
    | error e => rw [ha] at hexec; simp at hexec
    | ok nwA =>
      rw [ha] at hexec; simp only [] at hexec
      by_cases heq : haccess nwA vid = haccess nw vid
      · -- assign didn't change vid, rest did
        have hdiff' : haccess nw' vid ≠ haccess nwA vid := by rw [heq]; exact hdiff
        have hmem := trsVAssigns_writes_subset decls funcs ctxs cpos ifw nwA nw' rest hexec vid hdiff'
        simp only [assignsWrites]
        exact List.mem_append_right _ hmem
      · -- assign changed vid
        have hmem := trsVAssign_writes_subset decls funcs ctxs cpos ifw nw nwA a ha vid heq
        simp only [assignsWrites]
        exact List.mem_append_left _ hmem

-- Helper: trsVForStep writes are in forStepWrites.
private theorem trsVForStep_writes_subset (decls : Decls) (funcs : Funcs) (ctxs : State)
    (cpos : HPath) (ifw : IFW) (nw nw' : NW) (step : for_step)
    (hexec : trsVForStep decls funcs ctxs cpos ifw nw step = .ok nw') :
    forall vid, haccess nw' vid ≠ haccess nw vid -> vid ∈ forStepWrites step := by
  intro vid hdiff
  match step with
  | .op_assign (.op lv aop e) =>
    simp only [trsVForStep, bind, Except.bind] at hexec
    cases hlv : lvposfind decls funcs ctxs cpos ifw nw lv with
    | error e => rw [hlv] at hexec; simp at hexec
    | ok p =>
      rw [hlv] at hexec
      cases hev : evalExpr decls funcs ctxs cpos ifw nw e with
      | error e => rw [hev] at hexec; simp at hexec
      | ok ev =>
        rw [hev] at hexec; simp only [] at hexec
        have hpath := lvposfind_head_vid decls funcs ctxs cpos ifw nw lv p hlv
        cases hbop : assignOpToBinOp aop with
        | none =>
          simp [hbop, pure, Except.pure] at hexec
          rw [← hexec] at hdiff
          exact haccess_hadd_vid_mem_target nw lv p _ vid hpath hdiff
        | some bop =>
          simp only [hbop, bind, Except.bind] at hexec
          cases hf : hfind2 p nw ifw with
          | none => simp [hf] at hexec
          | some lval =>
            simp [hf, pure, Except.pure] at hexec
            rw [← hexec] at hdiff
            exact haccess_hadd_vid_mem_target nw lv p _ vid hpath hdiff
  | .inc_or_dec (.inc vid') =>
    simp only [trsVForStep, bind, Except.bind] at hexec
    cases hdf : declfind decls vid' with
    | error e => rw [hdf] at hexec; simp at hexec
    | ok p =>
      rw [hdf] at hexec
      cases hwf : wfind decls ifw nw vid' with
      | error e => rw [hwf] at hexec; simp at hexec
      | ok v =>
        rw [hwf] at hexec; simp [pure, Except.pure] at hexec
        rw [← hexec] at hdiff
        have hpeq := declfind_eq_vid hdf
        subst hpeq
        by_cases hc : vid = vid'
        · subst hc; exact List.Mem.head _
        · exact absurd (haccess_hadd_vid_cons_ne nw vid' [] _ vid hc) hdiff
  | .inc_or_dec (.dec vid') =>
    simp only [trsVForStep, bind, Except.bind] at hexec
    cases hdf : declfind decls vid' with
    | error e => rw [hdf] at hexec; simp at hexec
    | ok p =>
      rw [hdf] at hexec
      cases hwf : wfind decls ifw nw vid' with
      | error e => rw [hwf] at hexec; simp at hexec
      | ok v =>
        rw [hwf] at hexec; simp [pure, Except.pure] at hexec
        rw [← hexec] at hdiff
        have hpeq := declfind_eq_vid hdf
        subst hpeq
        by_cases hc : vid = vid'
        · subst hc; exact List.Mem.head _
        · exact absurd (haccess_hadd_vid_cons_ne nw vid' [] _ vid hc) hdiff

-- Helper: trsVForStep preserves WFHMap (all branches produce hadd ... (HMap.bits ...)).
private theorem trsVForStep_preserves_wf (decls : Decls) (funcs : Funcs) (ctxs : State)
    (cpos : HPath) (ifw : IFW) (nw nw' : NW) (step : for_step)
    (hexec : trsVForStep decls funcs ctxs cpos ifw nw step = .ok nw')
    (hwf : WFHMap nw) : WFHMap nw' := by
  match step with
  | .op_assign (.op lv aop e) =>
    simp only [trsVForStep, bind, Except.bind] at hexec
    cases hlv : lvposfind decls funcs ctxs cpos ifw nw lv with
    | error e => rw [hlv] at hexec; simp at hexec
    | ok p =>
      rw [hlv] at hexec
      cases hev : evalExpr decls funcs ctxs cpos ifw nw e with
      | error e => rw [hev] at hexec; simp at hexec
      | ok ev =>
        rw [hev] at hexec; simp only [] at hexec
        cases hbop : assignOpToBinOp aop with
        | none =>
          simp [hbop, pure, Except.pure] at hexec
          rw [← hexec]; exact WFHMap_hadd nw p _ hwf WFHMap.bits
        | some bop =>
          simp only [hbop, bind, Except.bind] at hexec
          cases hf : hfind2 p nw ifw with
          | none => simp [hf] at hexec
          | some lval =>
            simp [hf, pure, Except.pure] at hexec
            rw [← hexec]; exact WFHMap_hadd nw p _ hwf WFHMap.bits
  | .inc_or_dec (.inc vid) =>
    simp only [trsVForStep, bind, Except.bind] at hexec
    cases hdf : declfind decls vid with
    | error e => rw [hdf] at hexec; simp at hexec
    | ok p =>
      rw [hdf] at hexec
      cases hwfind : wfind decls ifw nw vid with
      | error e => rw [hwfind] at hexec; simp at hexec
      | ok v =>
        rw [hwfind] at hexec; simp [pure, Except.pure] at hexec
        rw [← hexec]; exact WFHMap_hadd nw p _ hwf WFHMap.bits
  | .inc_or_dec (.dec vid) =>
    simp only [trsVForStep, bind, Except.bind] at hexec
    cases hdf : declfind decls vid with
    | error e => rw [hdf] at hexec; simp at hexec
    | ok p =>
      rw [hdf] at hexec
      cases hwfind : wfind decls ifw nw vid with
      | error e => rw [hwfind] at hexec; simp at hexec
      | ok v =>
        rw [hwfind] at hexec; simp [pure, Except.pure] at hexec
        rw [← hexec]; exact WFHMap_hadd nw p _ hwf WFHMap.bits

-- Helper: trsVAssign preserves WFHMap.
private theorem trsVAssign_preserves_wf (decls : Decls) (funcs : Funcs) (ctxs : State)
    (cpos : HPath) (ifw : IFW) (nw nw' : NW) (a : assign)
    (hexec : trsVAssign decls funcs ctxs cpos ifw nw a = .ok nw')
    (hwf : WFHMap nw) : WFHMap nw' := by
  match a with
  | .net lv e =>
    simp only [trsVAssign, bind, Except.bind] at hexec
    cases hlv : lvposfind decls funcs ctxs cpos ifw nw lv with
    | error e => rw [hlv] at hexec; simp at hexec
    | ok p =>
      rw [hlv] at hexec
      cases hev : evalExpr decls funcs ctxs cpos ifw nw e with
      | error e => rw [hev] at hexec; simp at hexec
      | ok ev =>
        rw [hev] at hexec; simp [pure, Except.pure] at hexec
        rw [← hexec]; exact WFHMap_hadd nw p _ hwf WFHMap.bits

-- Helper: trsVAssigns preserves WFHMap.
private theorem trsVAssigns_preserves_wf (decls : Decls) (funcs : Funcs) (ctxs : State)
    (cpos : HPath) (ifw : IFW) (nw nw' : NW) (as_ : assigns)
    (hexec : trsVAssigns decls funcs ctxs cpos ifw nw as_ = .ok nw')
    (hwf : WFHMap nw) : WFHMap nw' := by
  match as_ with
  | .one a =>
    simp only [trsVAssigns] at hexec
    exact trsVAssign_preserves_wf decls funcs ctxs cpos ifw nw nw' a hexec hwf
  | .cons a rest =>
    simp only [trsVAssigns, bind, Except.bind] at hexec
    cases ha : trsVAssign decls funcs ctxs cpos ifw nw a with
    | error e => rw [ha] at hexec; simp at hexec
    | ok nwA =>
      rw [ha] at hexec; simp only [] at hexec
      have hwfA := trsVAssign_preserves_wf decls funcs ctxs cpos ifw nw nwA a ha hwf
      exact trsVAssigns_preserves_wf decls funcs ctxs cpos ifw nwA nw' rest hexec hwfA

set_option maxHeartbeats 800000 in
mutual
theorem trsVStatementItem_writes_subset (decls : Decls) (funcs : Funcs) (ctxs : State)
    (cpos : HPath) (ifw : IFW) (isComb : Bool) (sti : statement_item) (nw : NW)
    (updates flops retval : State)
    (hexec : trsVStatementItem decls funcs ctxs cpos ifw isComb sti nw =
             .ok (updates, flops, retval))
    (hwf : WFHMap nw)
    (hwf_pres : ∀ si nw' r, WFHMap nw' ->
        trsVStatementItem decls funcs ctxs cpos ifw isComb si nw' = .ok r -> WFHMap r.1) :
    ∀ vid, haccess updates vid ≠ haccess nw vid -> vid ∈ stmtWrites sti := by
  intro vid hdiff
  match sti with
  | .forever _ | .repeat _ _ | .while _ _ | .do_while _ _ =>
    simp only [trsVStatementItem] at hexec
    have : updates = nw := by
      cases hexec with | refl => rfl
    rw [this] at hdiff; exact absurd rfl hdiff
  | .return re =>
    unfold trsVStatementItem at hexec
    simp only [bind, Except.bind] at hexec
    cases heval : evalExpr decls funcs ctxs cpos ifw nw re with
    | error e => rw [heval] at hexec; simp at hexec
    | ok rv' =>
      rw [heval] at hexec
      simp [pure, Except.pure] at hexec
      rw [hexec.1] at hdiff; exact absurd rfl hdiff
  | .blocking_assign_normal lv e =>
    unfold trsVStatementItem at hexec
    simp only [bind, Except.bind] at hexec
    cases hlv : lvposfind decls funcs ctxs cpos ifw nw lv with
    | error e => rw [hlv] at hexec; simp at hexec
    | ok p =>
      rw [hlv] at hexec
      cases hev : evalExpr decls funcs ctxs cpos ifw nw e with
      | error e => rw [hev] at hexec; simp at hexec
      | ok ev =>
        rw [hev] at hexec
        simp [pure, Except.pure] at hexec
        rw [← hexec.1] at hdiff
        have hpath := lvposfind_head_vid decls funcs ctxs cpos ifw nw lv p hlv
        rw [sw1]
        exact haccess_hadd_vid_mem_target nw lv p _ vid hpath hdiff
  | .nonblocking_assign lv e =>
    unfold trsVStatementItem at hexec
    simp only [bind, Except.bind] at hexec
    cases hlv : lvposfind decls funcs ctxs cpos ifw nw lv with
    | error e => rw [hlv] at hexec; simp at hexec
    | ok p =>
      rw [hlv] at hexec
      cases hev : evalExpr decls funcs ctxs cpos ifw nw e with
      | error e => rw [hev] at hexec; simp at hexec
      | ok ev =>
        rw [hev] at hexec
        simp only [pure, Except.pure] at hexec
        cases isComb with
        | false =>
          simp at hexec
          rw [hexec.1] at hdiff; exact absurd rfl hdiff
        | true =>
          simp at hexec
          rw [← hexec.1] at hdiff
          have hpath := lvposfind_head_vid decls funcs ctxs cpos ifw nw lv p hlv
          rw [sw2]
          exact haccess_hadd_vid_mem_target nw lv p _ vid hpath hdiff
  | .case cty ce css =>
    unfold trsVStatementItem at hexec
    simp only [bind, Except.bind] at hexec
    cases hcv : evalExpr decls funcs ctxs cpos ifw nw ce with
    | error e => rw [hcv] at hexec; simp at hexec
    | ok cv =>
      rw [hcv] at hexec
      have hmem := trsVStatementCaseV_writes_subset decls funcs ctxs cpos ifw isComb
        cv css nw updates flops retval hexec hwf hwf_pres vid hdiff
      rw [sw3]; exact hmem
  | .cond cp ts fs =>
    unfold trsVStatementItem at hexec
    simp only [bind, Except.bind] at hexec
    cases hcv : evalExpr decls funcs ctxs cpos ifw nw cp with
    | error e => rw [hcv] at hexec; simp at hexec
    | ok cv =>
      rw [hcv] at hexec
      cases hzero : (hbits cv).isZero with
      | true =>
        simp [hzero] at hexec
        match hts : ts, hfs : fs with
        | some tsi, some (some fsi) =>
          have hmem := trsVStatementItem_writes_subset decls funcs ctxs cpos ifw isComb
            fsi nw updates flops retval hexec hwf hwf_pres vid hdiff
          rw [sw4]; exact List.mem_append_right _ hmem
        | some _, some none =>
          simp [pure, Except.pure] at hexec
          rw [hexec.1] at hdiff; exact absurd rfl hdiff
        | some _, none =>
          simp [pure, Except.pure] at hexec
          rw [hexec.1] at hdiff; exact absurd rfl hdiff
        | none, some (some fsi) =>
          have hmem := trsVStatementItem_writes_subset decls funcs ctxs cpos ifw isComb
            fsi nw updates flops retval hexec hwf hwf_pres vid hdiff
          rw [sw6]; exact List.mem_append_right _ hmem
        | none, some none =>
          simp [pure, Except.pure] at hexec
          rw [hexec.1] at hdiff; exact absurd rfl hdiff
        | none, none =>
          simp [pure, Except.pure] at hexec
          rw [hexec.1] at hdiff; exact absurd rfl hdiff
      | false =>
        simp [hzero] at hexec
        match hts : ts, hfs : fs with
        | some tsi, some (some fsi) =>
          have hmem := trsVStatementItem_writes_subset decls funcs ctxs cpos ifw isComb
            tsi nw updates flops retval hexec hwf hwf_pres vid hdiff
          rw [sw4]; exact List.mem_append_left _ hmem
        | some tsi, some none =>
          have hmem := trsVStatementItem_writes_subset decls funcs ctxs cpos ifw isComb
            tsi nw updates flops retval hexec hwf hwf_pres vid hdiff
          rw [sw5 cp (some none) tsi (by intro s h; cases h)]
          exact List.mem_append_left _ hmem
        | some tsi, none =>
          have hmem := trsVStatementItem_writes_subset decls funcs ctxs cpos ifw isComb
            tsi nw updates flops retval hexec hwf hwf_pres vid hdiff
          rw [sw5 cp none tsi (by intro s h; cases h)]
          exact List.mem_append_left _ hmem
        | none, _ =>
          simp [pure, Except.pure] at hexec
          rw [hexec.1] at hdiff; exact absurd rfl hdiff
  | .proc_timing_control ptc s =>
    unfold trsVStatementItem at hexec
    rw [sw14]
    exact trsVStatementItem_writes_subset decls funcs ctxs cpos ifw isComb s nw
      updates flops retval hexec hwf hwf_pres vid hdiff
  | .seq_block stis =>
    unfold trsVStatementItem at hexec
    have hmem := trsVStatementSeqBlock_writes_subset decls funcs ctxs cpos ifw isComb
      stis nw HMap.empty HMap.empty updates flops retval hexec hwf hwf_pres vid hdiff
    simp only [stmtWrites]; exact hmem
  | .for (.var_assigns fias) ce step body =>
    unfold trsVStatementItem at hexec
    simp only [bind, Except.bind] at hexec
    cases hinit : trsVAssigns decls funcs ctxs cpos ifw nw fias with
    | error e => rw [hinit] at hexec; simp at hexec
    | ok nw' =>
      rw [hinit] at hexec; simp only [] at hexec
      have hwf' := trsVAssigns_preserves_wf decls funcs ctxs cpos ifw nw nw' fias hinit hwf
      by_cases heqInit : haccess nw' vid = haccess nw vid
      · -- init didn't change vid, so the loop must have
        have hdiff' : haccess updates vid ≠ haccess nw' vid := by rw [heqInit]; exact hdiff
        have hmem := trsVStatementForLoop_writes_subset decls funcs ctxs cpos ifw isComb
          ce step body 32 nw' HMap.empty HMap.empty updates flops retval hexec hwf' hwf_pres vid hdiff'
        rw [sw11, List.append_assoc]
        exact List.mem_append_right _ hmem
      · -- init changed vid
        have hmem := trsVAssigns_writes_subset decls funcs ctxs cpos ifw nw nw' fias hinit vid heqInit
        rw [sw11, List.append_assoc]
        simp only [forInitWrites]
        exact List.mem_append_left _ hmem
termination_by (sizeOf sti, 0)

theorem trsVStatementSeqBlock_writes_subset (decls : Decls) (funcs : Funcs) (ctxs : State)
    (cpos : HPath) (ifw : IFW) (isComb : Bool)
    (stis : List statement_item) (nwInit : NW) (flInit rvInit : State)
    (updates flops retval : State)
    (hexec : trsVStatementSeqBlock decls funcs ctxs cpos ifw isComb stis nwInit flInit rvInit =
             .ok (updates, flops, retval))
    (hwf : WFHMap nwInit)
    (hwf_pres : ∀ si nw' r, WFHMap nw' ->
        trsVStatementItem decls funcs ctxs cpos ifw isComb si nw' = .ok r -> WFHMap r.1) :
    ∀ vid, haccess updates vid ≠ haccess nwInit vid -> vid ∈ stmtListWrites stis := by
  intro vid hdiff
  match stis with
  | [] =>
    simp only [trsVStatementSeqBlock, pure, Except.pure] at hexec
    cases hexec with | refl => exact absurd rfl hdiff
  | si :: rest =>
    simp only [trsVStatementSeqBlock, bind, Except.bind] at hexec
    cases hsi : trsVStatementItem decls funcs ctxs cpos ifw isComb si nwInit with
    | error e => rw [hsi] at hexec; simp at hexec
    | ok triple =>
      rw [hsi] at hexec
      obtain ⟨nw_si, fl_si, rv_si⟩ := triple
      simp only [] at hexec
      by_cases heq : haccess nw_si vid = haccess nwInit vid
      · -- nw_si agrees with nwInit at vid, so hupds preserves it
        have hnwAcc : haccess (hupds nwInit nw_si) vid = haccess nwInit vid :=
          haccess_hupds_eq_of_eq nwInit nw_si vid heq hwf
        have hdiff' : haccess updates vid ≠ haccess (hupds nwInit nw_si) vid := by
          rw [hnwAcc]; exact hdiff
        have hwf_nw_si : WFHMap nw_si := hwf_pres si nwInit (nw_si, fl_si, rv_si) hwf hsi
        have hwf' : WFHMap (hupds nwInit nw_si) := WFHMap_hupds nwInit nw_si hwf hwf_nw_si
        have hmem := trsVStatementSeqBlock_writes_subset decls funcs ctxs cpos ifw isComb
          rest (hupds nwInit nw_si) _ _ updates flops retval hexec hwf' hwf_pres vid hdiff'
        simp only [stmtListWrites]
        exact List.mem_append_right _ hmem
      · -- nw_si differs from nwInit at vid -> vid written by si
        have hmem := trsVStatementItem_writes_subset decls funcs ctxs cpos ifw isComb
          si nwInit nw_si fl_si rv_si hsi hwf hwf_pres vid heq
        simp only [stmtListWrites]
        exact List.mem_append_left _ hmem
termination_by (sizeOf stis, 0)

theorem trsVStatementCaseV_writes_subset (decls : Decls) (funcs : Funcs) (ctxs : State)
    (cpos : HPath) (ifw : IFW) (isComb : Bool)
    (cv : Value) (css : List (case_item statement_item)) (nw : NW)
    (updates flops retval : State)
    (hexec : trsVStatementCaseV decls funcs ctxs cpos ifw isComb cv css nw =
             .ok (updates, flops, retval))
    (hwf : WFHMap nw)
    (hwf_pres : ∀ si nw' r, WFHMap nw' ->
        trsVStatementItem decls funcs ctxs cpos ifw isComb si nw' = .ok r -> WFHMap r.1) :
    ∀ vid, haccess updates vid ≠ haccess nw vid -> vid ∈ caseItemListWrites css := by
  intro vid hdiff
  match css with
  | [] =>
    simp only [trsVStatementCaseV, pure, Except.pure] at hexec
    cases hexec with | refl => exact absurd rfl hdiff
  | (.default st) :: _ =>
    simp only [trsVStatementCaseV] at hexec
    have hmem := trsVStatementItem_writes_subset decls funcs ctxs cpos ifw isComb
      st nw updates flops retval hexec hwf hwf_pres vid hdiff
    simp only [caseItemListWrites]
    exact List.mem_append_left _ hmem
  | (.case ce st) :: rest =>
    simp only [trsVStatementCaseV, bind, Except.bind] at hexec
    cases hcev : evalExpr decls funcs ctxs cpos ifw nw ce with
    | error e => rw [hcev] at hexec; simp at hexec
    | ok cev =>
      rw [hcev] at hexec
      simp only [] at hexec
      cases hm : SZ.equiv (hbits cv) (hbits cev) with
      | true =>
        simp [hm] at hexec
        have hmem := trsVStatementItem_writes_subset decls funcs ctxs cpos ifw isComb
          st nw updates flops retval hexec hwf hwf_pres vid hdiff
        simp only [caseItemListWrites]
        exact List.mem_append_left _ hmem
      | false =>
        simp [hm] at hexec
        have hmem := trsVStatementCaseV_writes_subset decls funcs ctxs cpos ifw isComb
          cv rest nw updates flops retval hexec hwf hwf_pres vid hdiff
        simp only [caseItemListWrites]
        exact List.mem_append_right _ hmem
termination_by (sizeOf css, 0)

theorem trsVStatementForLoop_writes_subset (decls : Decls) (funcs : Funcs) (ctxs : State)
    (cpos : HPath) (ifw : IFW) (isComb : Bool)
    (ce : expression) (step : for_step) (body : statement_item)
    (fuel : Nat) (nwInit : NW) (flInit : Flops) (rvInit : Value)
    (updates flops retval : State)
    (hexec : trsVStatementForLoop decls funcs ctxs cpos ifw isComb
               ce step body fuel nwInit flInit rvInit = .ok (updates, flops, retval))
    (hwf : WFHMap nwInit)
    (hwf_pres : forall si nw' r, WFHMap nw' ->
        trsVStatementItem decls funcs ctxs cpos ifw isComb si nw' = .ok r -> WFHMap r.1) :
    forall vid, haccess updates vid ≠ haccess nwInit vid ->
      vid ∈ forStepWrites step ++ stmtWrites body := by
  intro vid hdiff
  match fuel with
  | 0 =>
    simp only [trsVStatementForLoop, pure, Except.pure] at hexec
    cases hexec with | refl => exact absurd rfl hdiff
  | Nat.succ fuel' =>
    simp only [trsVStatementForLoop, bind, Except.bind] at hexec
    cases hcv : evalExpr decls funcs ctxs cpos ifw nwInit ce with
    | error e => rw [hcv] at hexec; simp at hexec
    | ok cv =>
      rw [hcv] at hexec; simp only [] at hexec
      cases hzero : (hbits cv).isZero with
      | true =>
        simp only [hzero, ite_true, pure, Except.pure] at hexec
        obtain ⟨rfl, _, _⟩ := hexec
        exact absurd rfl hdiff
      | false =>
        simp only [hzero, ite_false, bind, Except.bind] at hexec
        cases hbody : trsVStatementItem decls funcs ctxs cpos ifw isComb body nwInit with
        | error e => rw [hbody] at hexec; simp at hexec
        | ok triple =>
          rw [hbody] at hexec
          obtain ⟨nwB, flB, rvB⟩ := triple
          simp only [] at hexec
          -- nw'' = hupds nwInit nwB
          -- nw''' = result of trsVForStep on nw''
          cases hstep : trsVForStep decls funcs ctxs cpos ifw (hupds nwInit nwB) step with
          | error e => rw [hstep] at hexec; simp at hexec
          | ok nwS =>
            rw [hstep] at hexec; simp only [] at hexec
            -- hexec is now about the recursive call with fuel'
            -- We need to figure out if vid was changed by body+step or by the recursive loop
            have hwfB : WFHMap nwB := hwf_pres body nwInit (nwB, flB, rvB) hwf hbody
            have hwfUpd : WFHMap (hupds nwInit nwB) := WFHMap_hupds nwInit nwB hwf hwfB
            by_cases heqS : haccess nwS vid = haccess nwInit vid
            · -- step result agrees with nwInit at vid -> recursive loop changed it
              have hdiff' : haccess updates vid ≠ haccess nwS vid := by rw [heqS]; exact hdiff
              -- Need WFHMap nwS for the recursive call
              -- nwS is result of trsVForStep which does hadd, preserving WFHMap
              -- For now, we use the fact that we can recurse
              exact trsVStatementForLoop_writes_subset decls funcs ctxs cpos ifw isComb
                ce step body fuel' nwS _ _ updates flops retval hexec
                (trsVForStep_preserves_wf decls funcs ctxs cpos ifw (hupds nwInit nwB) nwS step hstep hwfUpd)
                hwf_pres vid hdiff'
            · -- nwS differs from nwInit at vid
              -- Either hupds nwInit nwB differs from nwInit (body changed it)
              -- or nwS differs from hupds nwInit nwB (step changed it)
              by_cases heqUpd : haccess (hupds nwInit nwB) vid = haccess nwInit vid
              · -- body didn't change it through hupds, so step changed it
                have hdiffStep : haccess nwS vid ≠ haccess (hupds nwInit nwB) vid := by
                  rw [heqUpd]; exact heqS
                have hmem := trsVForStep_writes_subset decls funcs ctxs cpos ifw
                  (hupds nwInit nwB) nwS step hstep vid hdiffStep
                exact List.mem_append_left _ hmem
              · -- body changed it (via hupds)
                -- haccess (hupds nwInit nwB) vid ≠ haccess nwInit vid
                -- This means haccess nwB vid ≠ haccess nwInit vid (by contrapositive of haccess_hupds_eq_of_eq)
                have hneqB : haccess nwB vid ≠ haccess nwInit vid := by
                  intro heqB
                  exact heqUpd (haccess_hupds_eq_of_eq nwInit nwB vid heqB hwf)
                have hmem := trsVStatementItem_writes_subset decls funcs ctxs cpos ifw isComb
                  body nwInit nwB flB rvB hbody hwf hwf_pres vid hneqB
                exact List.mem_append_right _ hmem
termination_by (sizeOf body, fuel)
end

-- hupds_comm_disjoint is defined below (haccess_hupds was moved above)

/- Fixed point of hupds determines field values.
    If hupds sf u = sf and haccess u vid ≠ .empty, then haccess sf vid = haccess u vid.

    Proof: By haccess_hupds, haccess sf vid = hupds (haccess sf vid) (haccess u vid).
    Then hupds_fixpt_eq_leaf (with SameShape and IsLeaf) gives the result.

    The IsLeaf hypothesis says that the update field is a leaf (bits) value.
    In the Verilog semantics, process updates always write leaf values to
    individual variables, so this always holds. -/
theorem hupds_fixpt_determines (sf u : HMap) (vid : VId)
    (hfp : hupds sf u = sf)
    (hne : haccess u vid ≠ HMap.empty)
    (hss : HMapLemmas.SameShape (haccess sf vid) (haccess u vid))
    (hleaf : HMapLemmas.IsLeaf (haccess u vid))
    (hcompat : CompatUpdate sf u) :
    haccess sf vid = haccess u vid := by
  have h := haccess_hupds sf u vid hcompat
  rw [hfp] at h
  -- h: haccess sf vid = hupds (haccess sf vid) (haccess u vid)
  exact hupds_fixpt_eq_leaf (haccess sf vid) (haccess u vid) h.symm hne hss hleaf

-- ## hupds with disjoint maps commutes

-- Helper: hupds (hupds x y) .empty = hupds x y.
private theorem hupds_hupds_empty_right (x y : HMap) :
    hupds (hupds x y) .empty = hupds x y := by
  cases hupds x y <;> rfl

-- Helper: hupds (hupds x .empty) y = hupds x y.
private theorem hupds_hupds_empty_mid (x y : HMap) :
    hupds (hupds x .empty) y = hupds x y := by
  rw [hupds_empty_right]

/- Helper: for single-field case, disjoint hupds commutes.
    If y = .empty or z = .empty, hupds (hupds x y) z = hupds (hupds x z) y. -/
private theorem hupds_comm_at_most_one (x y z : HMap)
    (h : y ≠ .empty → z = .empty) :
    hupds (hupds x y) z = hupds (hupds x z) y := by
  by_cases hy : y = .empty
  · rw [hy, hupds_hupds_empty_mid, hupds_hupds_empty_right]
  · rw [h hy, hupds_hupds_empty_right, hupds_hupds_empty_mid]

/- hupds with disjoint maps commutes.
    Proved at the per-field (haccess) level using haccess_hupds, then
    lifted to structural equality using hupds_eq_of_haccess_agree.

    Requires structural hypotheses: s, a, b are all .str with a having
    contained keys and s having no duplicate keys. These always hold
    in the Verilog semantics. -/
theorem hupds_comm_disjoint
    (s_fields a_fields b_fields : List (String × HMap))
    (hdisjoint : ∀ v, haccess (.str a_fields) v ≠ HMap.empty →
        haccess (.str b_fields) v ≠ HMap.empty → False)
    (hnodup_s : NoDupKeys s_fields)
    (hkeys_a : ∀ k, k ∈ a_fields.map Prod.fst → k ∈ s_fields.map Prod.fst)
    (hkeys_b : ∀ k, k ∈ b_fields.map Prod.fst → k ∈ s_fields.map Prod.fst) :
    hupds (hupds (.str s_fields) (.str a_fields)) (.str b_fields) =
    hupds (hupds (.str s_fields) (.str b_fields)) (.str a_fields) := by
  -- Both sides are .str, so we can use hupds_eq_of_haccess_agree-style reasoning.
  -- First, establish per-field equality.
  let s := HMap.str s_fields
  let a := HMap.str a_fields
  let b := HMap.str b_fields
  have hcompat_sa : CompatUpdate s a := by simp [CompatUpdate]
  have hcompat_sb : CompatUpdate s b := by simp [CompatUpdate]
  -- hupds (.str _) (.str _) = .str _, so intermediate results are .str
  have hsa_str : ∃ fs, hupds s a = .str fs := ⟨_, rfl⟩
  have hsb_str : ∃ fs, hupds s b = .str fs := ⟨_, rfl⟩
  obtain ⟨sa_fs, hsa_eq⟩ := hsa_str
  obtain ⟨sb_fs, hsb_eq⟩ := hsb_str
  have hcompat_sab : CompatUpdate (hupds s a) b := by rw [hsa_eq]; simp [CompatUpdate]
  have hcompat_sba : CompatUpdate (hupds s b) a := by rw [hsb_eq]; simp [CompatUpdate]
  -- Per-field equality
  have hfield : ∀ vid, haccess (hupds (hupds s a) b) vid =
      haccess (hupds (hupds s b) a) vid := by
    intro vid
    rw [haccess_hupds _ _ _ hcompat_sab, haccess_hupds _ _ _ hcompat_sa,
        haccess_hupds _ _ _ hcompat_sba, haccess_hupds _ _ _ hcompat_sb]
    exact hupds_comm_at_most_one (haccess s vid) (haccess a vid) (haccess b vid)
      (fun hne =>
        Classical.byContradiction (fun hne2 => hdisjoint vid hne hne2))
  -- Step 1: show hbinUStr2 is empty for all our hupds applications
  have hbin_a : hbinUStr2 s_fields a_fields = [] :=
    hbinUStr2_empty_of_keys_subset s_fields a_fields
      (fun k v hm => any_key_of_mem_map (hkeys_a k (List.mem_map.mpr ⟨(k, v), hm, rfl⟩)))
  have hbin_b : hbinUStr2 s_fields b_fields = [] :=
    hbinUStr2_empty_of_keys_subset s_fields b_fields
      (fun k v hm => any_key_of_mem_map (hkeys_b k (List.mem_map.mpr ⟨(k, v), hm, rfl⟩)))
  -- Step 2: simplify hupds (.str s_fields) (.str X) = .str (hupdsStr s_fields X)
  have hsa_simp : hupds (.str s_fields) (.str a_fields) = .str (hupdsStr s_fields a_fields) := by
    simp [hupds, hbin_a]
  have hsb_simp : hupds (.str s_fields) (.str b_fields) = .str (hupdsStr s_fields b_fields) := by
    simp [hupds, hbin_b]
  -- Step 3: keys of hupdsStr are the same as s_fields
  have hkeys_sa : (hupdsStr s_fields a_fields).map Prod.fst = s_fields.map Prod.fst :=
    hupdsStr_keys s_fields a_fields
  have hkeys_sb : (hupdsStr s_fields b_fields).map Prod.fst = s_fields.map Prod.fst :=
    hupdsStr_keys s_fields b_fields
  -- Step 4: NoDupKeys for intermediate results
  have hnodup_sa : NoDupKeys (hupdsStr s_fields a_fields) := by
    show (hupdsStr s_fields a_fields).map Prod.fst |>.Nodup
    rw [hkeys_sa]; exact hnodup_s
  have hnodup_sb : NoDupKeys (hupdsStr s_fields b_fields) := by
    show (hupdsStr s_fields b_fields).map Prod.fst |>.Nodup
    rw [hkeys_sb]; exact hnodup_s
  -- Step 5: hkeys for b into hupdsStr s_fields a_fields (same key set as s_fields)
  have hkeys_b_in_sa : ∀ k, k ∈ b_fields.map Prod.fst → k ∈ (hupdsStr s_fields a_fields).map Prod.fst := by
    intro k hk; rw [hkeys_sa]; exact hkeys_b k hk
  have hkeys_a_in_sb : ∀ k, k ∈ a_fields.map Prod.fst → k ∈ (hupdsStr s_fields b_fields).map Prod.fst := by
    intro k hk; rw [hkeys_sb]; exact hkeys_a k hk
  -- Step 6: hbinUStr2 for the second layer is also empty
  have hbin_b2 : hbinUStr2 (hupdsStr s_fields a_fields) b_fields = [] :=
    hbinUStr2_empty_of_keys_subset _ _
      (fun k v hm => any_key_of_mem_map (hkeys_b_in_sa k (List.mem_map.mpr ⟨(k, v), hm, rfl⟩)))
  have hbin_a2 : hbinUStr2 (hupdsStr s_fields b_fields) a_fields = [] :=
    hbinUStr2_empty_of_keys_subset _ _
      (fun k v hm => any_key_of_mem_map (hkeys_a_in_sb k (List.mem_map.mpr ⟨(k, v), hm, rfl⟩)))
  -- Step 7: simplify the full expressions
  -- LHS = hupds (.str (hupdsStr s_fields a_fields)) (.str b_fields)
  --      = .str (hupdsStr (hupdsStr s_fields a_fields) b_fields)
  -- RHS = .str (hupdsStr (hupdsStr s_fields b_fields) a_fields)
  have hlhs : hupds (hupds (.str s_fields) (.str a_fields)) (.str b_fields) =
      .str (hupdsStr (hupdsStr s_fields a_fields) b_fields) := by
    rw [hsa_simp]; simp [hupds, hbin_b2]
  have hrhs : hupds (hupds (.str s_fields) (.str b_fields)) (.str a_fields) =
      .str (hupdsStr (hupdsStr s_fields b_fields) a_fields) := by
    rw [hsb_simp]; simp [hupds, hbin_a2]
  rw [hlhs, hrhs]
  -- Step 8: Both are .str with the same keys (from hupdsStr_keys)
  -- and agree on all haccessV (from hfield). Use list extensionality.
  have hkeys_lhs : (hupdsStr (hupdsStr s_fields a_fields) b_fields).map Prod.fst = s_fields.map Prod.fst := by
    rw [hupdsStr_keys, hkeys_sa]
  have hkeys_rhs : (hupdsStr (hupdsStr s_fields b_fields) a_fields).map Prod.fst = s_fields.map Prod.fst := by
    rw [hupdsStr_keys, hkeys_sb]
  have hnodup_lhs : NoDupKeys (hupdsStr (hupdsStr s_fields a_fields) b_fields) := by
    show (hupdsStr (hupdsStr s_fields a_fields) b_fields).map Prod.fst |>.Nodup
    rw [hkeys_lhs]; exact hnodup_s
  -- Show the two field lists are equal by using hupdsStr_id_of_haccessV_eq
  congr 1
  -- Need: two lists with same keys (same Prod.fst map), NoDupKeys, and same haccessV are equal
  -- Extract haccessV agreement from hfield
  have hval_eq : ∀ vid, haccessV vid (hupdsStr (hupdsStr s_fields a_fields) b_fields) =
      haccessV vid (hupdsStr (hupdsStr s_fields b_fields) a_fields) := by
    intro vid
    have := hfield vid
    rw [hlhs, hrhs] at this
    simp [haccess] at this
    exact this
  -- Both lists have NoDupKeys and the same key map.
  -- Prove equality by induction using the NoDupKeys + same keys + same values.
  exact assocList_ext_of_nodup_keys
    (hupdsStr (hupdsStr s_fields a_fields) b_fields)
    (hupdsStr (hupdsStr s_fields b_fields) a_fields)
    hnodup_lhs (hkeys_rhs.trans hkeys_lhs.symm) hval_eq

-- ## wfind is insensitive to hupds on non-overlapping variables

-- hpos always returns paths of the form [HElt.vid vid].
private theorem hpos_eq_vid {vid : String} {fs : List (String × HMap)} {p : HPath}
    (h : hpos vid fs = some p) : p = [HElt.vid vid] := by
  induction fs with
  | nil => simp [hpos] at h
  | cons entry rest ih =>
    obtain ⟨k, v⟩ := entry
    simp only [hpos] at h
    by_cases hkv : vid = k
    · subst hkv; simp at h; exact h.symm
    · have hne : (vid == k) = false := by simp [BEq.beq]; exact hkv
      rw [hne] at h; exact ih h

-- hfind on a single-step vid path reduces to haccess.
private theorem hfind_vid (h : HMap) (vid : VId) : hfind h [HElt.vid vid] = haccess h vid := by
  simp [hfind, hmove]

/- If vid is not in the update set (haccess updates vid = empty),
    then wfind gives the same result before and after hupds.
    Proved from haccess_hupds_miss + hpos_eq_vid + hfind_vid. -/
theorem wfind_hupds_miss (decls : Decls) (ifw : IFW) (nw updates : NW) (vid : VId)
    (hmiss : haccess updates vid = HMap.empty)
    (hcompat : CompatUpdate nw updates) :
    wfind decls ifw (hupds nw updates) vid = wfind decls ifw nw vid := by
  show (do let p ← declfind decls vid
           match hfind2 p (hupds nw updates) ifw with
           | some v => pure v
           | none => Except.error TrsFail.undriven) =
       (do let p ← declfind decls vid
           match hfind2 p nw ifw with
           | some v => pure v
           | none => Except.error TrsFail.undriven)
  cases hdecl : declfind decls vid with
  | error e => rfl
  | ok p =>
    show (match hfind2 p (hupds nw updates) ifw with | some v => pure v | none => Except.error TrsFail.undriven) =
         (match hfind2 p nw ifw with | some v => pure v | none => Except.error TrsFail.undriven)
    have hp : hpos vid (hstr decls) = some p := by
      simp only [declfind, VerilLean.Lang.Semantics.liftOption] at hdecl
      split at hdecl <;> simp_all
    have hpeq := hpos_eq_vid hp; subst hpeq
    simp only [hfind2, hfind_vid]
    rw [haccess_hupds_miss nw updates vid hmiss hcompat]

-- ## Disjoint-writes lemma (proved from wfind_hupds_miss)

-- Disjoint writes don't interfere (version 2, using wfind_hupds_miss).
theorem disjoint_writes_preserve_reads'
    (decls : Decls) (ifw : IFW) (nw updates : NW)
    (reads writes : List VId)
    (hdisjoint : ∀ v, v ∈ writes → v ∈ reads → False)
    (hupdates : ∀ vid, haccess updates vid ≠ HMap.empty → vid ∈ writes)
    (hcompat : CompatUpdate nw updates) :
    StatesAgreeOn decls ifw ifw nw (hupds nw updates) reads := by
  intro v hv
  -- Need: wfind decls ifw nw v = wfind decls ifw (hupds nw updates) v
  -- wfind_hupds_miss gives the other direction, so use symm
  have hmiss : haccess updates v = HMap.empty :=
    Classical.byContradiction fun h => hdisjoint v (hupdates v h) hv
  exact (wfind_hupds_miss decls ifw nw updates v hmiss hcompat).symm

end VerilLean.Lang.Equiv.EvalFrame
