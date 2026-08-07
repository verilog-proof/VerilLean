-- Module transition system helpers

import VerilLean.Lib.Lib
import VerilLean.Lang.Semantics

namespace VerilLean.Lang.ModuleTrs

open VerilLean.Lib
open VerilLean.Lang.Semantics
open VerilLean.Lang.Syntax (VId)

structure ModulePkg where
  mtrs   : MTrs
  rstVid : VId
  rstNeg : Bool -- `true` if negative reset

def buildPreReset (mpkg : ModulePkg) : State :=
  ⟨[(mpkg.rstVid, HMap.bits (SZ.mk' (if mpkg.rstNeg then 0 else 1) 1 false))]⟩

def buildReset (mpkg : ModulePkg) (emptyInputs initialFlops : State) : State :=
  initialFlops.merge
    (mpkg.mtrs.func (emptyInputs.merge (buildPreReset mpkg)) initialFlops).1

end VerilLean.Lang.ModuleTrs
