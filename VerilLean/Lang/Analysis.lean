-- I/O port analysis — extracting input/output port identifiers from module declarations.

import VerilLean.Lang.Syntax
import VerilLean.Lib.Common

namespace VerilLean.Lang.Analysis

open VerilLean.Lang.Syntax
open VerilLean.Lib (o2l ocons)

def ioAnsiPortDecl (pdecl : ansi_port_decl) : Option VId × Option VId :=
  match pdecl with
  | .net none _ => (none, none)
  | .net (some (.net none _)) _ => (none, none)
  | .net (some (.net (some pdir) _)) vid =>
      match pdir with
      | .input => (some vid, none)
      | .output => (none, some vid)
      | _ => (none, none)
  | .var none _ _ => (none, none)
  | .var (some (.var none _)) _ _ => (none, none)
  | .var (some (.var (some pdir) _)) vid _ =>
      match pdir with
      | .input => (some vid, none)
      | .output => (none, some vid)
      | _ => (none, none)

def ioAnsiPortDecls (pdecls : ansi_port_decls) : List VId × List VId :=
  match pdecls with
  | .nil => ([], [])
  | .one pdecl =>
      let (oi, oo) := ioAnsiPortDecl pdecl
      (o2l oi, o2l oo)
  | .cons pdecl spdecls =>
      let (ih, oh) := ioAnsiPortDecl pdecl
      let (it, ot) := ioAnsiPortDecls spdecls
      (ocons ih it, ocons oh ot)

def ioPortIds (pids : port_ids) : List VId :=
  match pids with
  | .one vid => [vid]
  | .cons vid spids => vid :: ioPortIds spids

def ioPortDecl (pd : port_decl) : List VId × List VId :=
  match pd with
  | .input_p _ pids => (ioPortIds pids, [])
  | .input_d _ pids => (ioPortIds pids, [])
  | .output_p _ pids => ([], ioPortIds pids)
  | .output_d _ pids => ([], ioPortIds pids)
  | _ => ([], [])

def ioModuleItem (mitem : module_item) : List VId × List VId :=
  match mitem with
  | .port_decl pd => ioPortDecl pd
  | .non_port _ => ([], [])

def ioModuleItems (mitems : module_items) : List VId × List VId :=
  match mitems with
  | .one mitem => ioModuleItem mitem
  | .cons mitem mitems' =>
      let (ih, oh) := ioModuleItem mitem
      let (it, ot) := ioModuleItems mitems'
      (ih ++ it, oh ++ ot)

def getIOIds (m : module_decl) : List VId × List VId :=
  match m with
  | .ansi _ _ pdecls mitems =>
      let (ideclsP, odeclsP) := ioAnsiPortDecls pdecls
      let (ideclsI, odeclsI) := ioModuleItems mitems
      (ideclsP ++ ideclsI, odeclsP ++ odeclsI)

end VerilLean.Lang.Analysis
