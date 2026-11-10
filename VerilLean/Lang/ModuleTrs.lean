-- Module transition system helpers

import VerilLean.Lib.Lib
import VerilLean.Lang.Semantics
import VerilLean.Lang.Analysis

namespace VerilLean.Lang.ModuleTrs

open VerilLean.Lib
open VerilLean.Lang.Semantics
open VerilLean.Lang.Syntax (VId module_decl)

structure ModulePkg where
  mtrs   : MTrs
  rstVid : VId
  rstNeg : Bool -- `true` if negative reset

def buildPreReset (mpkg : ModulePkg) : State :=
  ⟨[(mpkg.rstVid, HMap.bits (SZ.mk' (if mpkg.rstNeg then 0 else 1) 1 false))]⟩

def buildReset (mpkg : ModulePkg) (emptyInputs initialFlops : State) : State :=
  initialFlops.merge
    (mpkg.mtrs.func (emptyInputs.merge (buildPreReset mpkg)) initialFlops).1

/-
build MTrs and add a check for missing inputs. If there are missing inputs, turn on partialInputs and run to return outs that can be evaluated
-/
def buildMTrs (m : module_decl) (mtrss : MTrss) : trsOk MTrs := do
  let ctx ← moduleCtxVModuleDecl [] m
  let (ins, outs) := VerilLean.Lang.Analysis.getIOIds m
  pure { inputVids := ins,
         inputDecls := ctx.decls,
         outputVids := outs,
         func := fun i fl =>
           let missing := ins.any (fun name => (i.get? name).isNone)
           let ctx' := if missing then { ctx with partialInputs := true } else ctx
           match trsT ctx' mtrss [] m outs i fl with
           | .ok (o, fl') => (o, fl')
           | .error _ => (State.empty, fl) }

end VerilLean.Lang.ModuleTrs
