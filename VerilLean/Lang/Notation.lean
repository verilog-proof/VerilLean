/- # Verilog-like notation for the VerilLean AST.
   Uses Lean 4 syntax extensions: `declare_syntax_cat` + `syntax` + `macro_rules`.
   Entry point: `v![ module ... endmodule ]`

   NOTE: Verilog's `'` (tick) based literals conflict with Lean's lexer.
   We use `#` as the tick replacement, e.g.,
     `4'b0101` → `4#b0101`
     `'0` → `#0`
     `'1` → `#1`
-/

import VerilLean.Lang.Syntax

namespace VerilLean.Lang.Notation

open VerilLean.Lang.Syntax

-- ## Coercions

instance : Coe assign assigns where coe := .one
instance : Coe var_decl_assign var_decl_assigns where coe := .one
instance : Coe param_assign param_assigns where coe := .one
instance : Coe net_decl_assign net_decl_assigns where coe := .one
instance : Coe named_port_conn named_port_conns where coe := .one
instance : Coe ansi_port_decl ansi_port_decls where coe := .one
instance : Coe param_decl param_ports where coe := .one
instance : Coe module_item module_items where coe := .one
instance : Coe dim packed_dims where coe := .one

instance : Coe integral_number number where coe := .integral
instance : Coe decimal_number integral_number where coe := .decimal
instance : Coe number primary_literal where coe := .number
instance : Coe unbased_unsized_literal primary_literal where coe := .unbased_unsized
instance : Coe expression constant_param_expression where coe := .min_typ_max
instance : Coe event_expression event_control where coe := .expr
instance : Coe event_control proc_timing_control where coe := .event
instance : Coe statement_item statement where coe := .stmt
instance : Coe assigns for_init where coe := .var_assigns
instance : Coe op_assign for_step where coe := .op_assign
instance : Coe inc_or_dec_expression for_step where coe := .inc_or_dec
instance : Coe var_decl data_decl where coe := .var_decl
instance : Coe named_port_conns port_conns where coe := .named
instance : Coe cont_assign module_common_item where coe := .cont_assign
instance : Coe concur_assert module_common_item where coe := .assert
instance : Coe module_gen_item_decl module_common_item where coe := .decl
instance : Coe pkg_gen_item_decl module_gen_item_decl where coe := .pkg
instance : Coe net_decl pkg_gen_item_decl where coe := .net
instance : Coe data_decl pkg_gen_item_decl where coe := .data
instance : Coe task_decl pkg_gen_item_decl where coe := .task
instance : Coe func_decl pkg_gen_item_decl where coe := .func
instance : Coe param_decl pkg_gen_item_decl where coe := .param
instance : Coe local_param_decl pkg_gen_item_decl where coe := .local_param
instance : Coe module_common_item module_or_generate_item where coe := .common
instance : Coe module_ins module_or_generate_item where coe := .ins
instance : Coe generated_module_ins non_port_module_item where coe := .generated_module_ins
instance : Coe module_or_generate_item non_port_module_item where coe := .module_or_generate_item
instance : Coe port_decl module_item where coe := .port_decl
instance : Coe non_port_module_item module_item where coe := .non_port

-- ## Syntax categories

declare_syntax_cat vexpr
declare_syntax_cat vpexpr
declare_syntax_cat vstmt
declare_syntax_cat vstmt_event
declare_syntax_cat vcase_item
declare_syntax_cat vassign
declare_syntax_cat vblkassign
declare_syntax_cat vnetdecl
declare_syntax_cat vvardecl
declare_syntax_cat vparamassign
declare_syntax_cat vpackeddim
declare_syntax_cat vport
declare_syntax_cat vparamport
declare_syntax_cat vportconn
declare_syntax_cat vportconnid
declare_syntax_cat vmoditem
declare_syntax_cat vgen
declare_syntax_cat vtop

-- Allow custom syntax categories to be spliced into term quotations in macros
instance : Coe (Lean.TSyntax `vexpr) (Lean.TSyntax `term) where coe s := ⟨s.raw⟩
instance : Coe (Lean.TSyntax `vpexpr) (Lean.TSyntax `term) where coe s := ⟨s.raw⟩
instance : Coe (Lean.TSyntax `vstmt) (Lean.TSyntax `term) where coe s := ⟨s.raw⟩
instance : Coe (Lean.TSyntax `vstmt_event) (Lean.TSyntax `term) where coe s := ⟨s.raw⟩
instance : Coe (Lean.TSyntax `vcase_item) (Lean.TSyntax `term) where coe s := ⟨s.raw⟩
instance : Coe (Lean.TSyntax `vassign) (Lean.TSyntax `term) where coe s := ⟨s.raw⟩
instance : Coe (Lean.TSyntax `vblkassign) (Lean.TSyntax `term) where coe s := ⟨s.raw⟩
instance : Coe (Lean.TSyntax `vnetdecl) (Lean.TSyntax `term) where coe s := ⟨s.raw⟩
instance : Coe (Lean.TSyntax `vvardecl) (Lean.TSyntax `term) where coe s := ⟨s.raw⟩
instance : Coe (Lean.TSyntax `vparamassign) (Lean.TSyntax `term) where coe s := ⟨s.raw⟩
instance : Coe (Lean.TSyntax `vpackeddim) (Lean.TSyntax `term) where coe s := ⟨s.raw⟩
instance : Coe (Lean.TSyntax `vport) (Lean.TSyntax `term) where coe s := ⟨s.raw⟩
instance : Coe (Lean.TSyntax `vparamport) (Lean.TSyntax `term) where coe s := ⟨s.raw⟩
instance : Coe (Lean.TSyntax `vportconn) (Lean.TSyntax `term) where coe s := ⟨s.raw⟩
instance : Coe (Lean.TSyntax `vportconnid) (Lean.TSyntax `term) where coe s := ⟨s.raw⟩
instance : Coe (Lean.TSyntax `vmoditem) (Lean.TSyntax `term) where coe s := ⟨s.raw⟩
instance : Coe (Lean.TSyntax `vgen) (Lean.TSyntax `term) where coe s := ⟨s.raw⟩
instance : Coe (Lean.TSyntax `vtop) (Lean.TSyntax `term) where coe s := ⟨s.raw⟩

-- ## vexpr: Expressions (includes literals inline)

-- Literal atoms: unbased unsized (using # instead of ' to avoid Lean lexer conflicts)
syntax:100 "#0" : vexpr                                       -- '0 (all zeros)
syntax:100 "#1" : vexpr                                       -- '1 (all ones)

-- Decimal literal (no base)
syntax:100 num : vexpr

-- Sized/unsized binary: 4#b0101 or #b0101
syntax:100 "#b" num : vexpr
syntax:100 num "#b" num : vexpr

-- Sized/unsized octal
syntax:100 "#o" num : vexpr
syntax:100 num "#o" num : vexpr

-- Sized/unsized hex
syntax:100 "#h" num : vexpr
syntax:100 num "#h" num : vexpr

-- Sized/unsized decimal base
syntax:100 "#d" num : vexpr
syntax:100 num "#d" num : vexpr

-- Identifier
syntax:100 ident : vexpr

-- Parenthesized
syntax:100 "(" vexpr ")" : vexpr

-- Select / range
syntax:74 vexpr "[" vexpr "]" : vexpr
syntax:74 vexpr "[" vexpr ":" vexpr "]" : vexpr
syntax:74 vexpr "[" vexpr "+:" vexpr "]" : vexpr
syntax:74 vexpr "[" vexpr "-:" vexpr "]" : vexpr

-- Hierarchical access
syntax:72 vexpr "·" vexpr : vexpr

-- Concatenation / replication
syntax:70 "{" sepBy(vexpr, ",") "}" : vexpr
syntax:70 "{" vexpr "{" sepBy1(vexpr, ",") "}" "}" : vexpr

-- Cast: sz#(e)
syntax:70 vexpr "#(" vexpr ")" : vexpr

-- Function / system task calls
syntax:69 ident "(" sepBy1(vexpr, ",") ")" : vexpr
syntax:69 "$signed" "(" vexpr ")" : vexpr
syntax:69 "$unsigned" "(" vexpr ")" : vexpr

-- Increment / decrement (using dec! instead of -- which conflicts with Lean comments)
syntax:78 "++" ident : vexpr
syntax:78 ident "++" : vexpr
syntax:78 "dec!" ident : vexpr
syntax:78 ident "dec!" : vexpr

-- Unary operators
syntax:78 "﹢" vexpr : vexpr    -- unary plus (using fullwidth to disambiguate)
syntax:78 "﹣" vexpr : vexpr    -- unary minus
syntax:77 "!" vexpr : vexpr
syntax:77 "~" vexpr : vexpr
syntax:77 "u&" vexpr : vexpr     -- unary reduction and
syntax:77 "u~&" vexpr : vexpr
syntax:77 "u|" vexpr : vexpr     -- unary reduction or
syntax:77 "u~|" vexpr : vexpr
syntax:77 "u^" vexpr : vexpr     -- unary reduction xor
syntax:77 "u~^" vexpr : vexpr
syntax:77 "u^~" vexpr : vexpr

-- Binary operators (highest to lowest precedence)
syntax:82 vexpr "**" vexpr : vexpr
syntax:83 vexpr "*" vexpr : vexpr
syntax:83 vexpr "/" vexpr : vexpr
syntax:83 vexpr "%" vexpr : vexpr
syntax:84 vexpr "+" vexpr : vexpr
syntax:84 vexpr "-" vexpr : vexpr
syntax:85 vexpr ">>" vexpr : vexpr
syntax:85 vexpr "<<" vexpr : vexpr
syntax:85 vexpr ">>>" vexpr : vexpr
syntax:85 vexpr "<<<" vexpr : vexpr
syntax:86 vexpr "<" vexpr : vexpr
syntax:86 vexpr "<=" vexpr : vexpr
syntax:86 vexpr ">" vexpr : vexpr
syntax:86 vexpr ">=" vexpr : vexpr
syntax:86 vexpr "inside" "{" sepBy1(vexpr, ",") "}" : vexpr
syntax:87 vexpr "==" vexpr : vexpr
syntax:87 vexpr "!=" vexpr : vexpr
syntax:88 vexpr "===" vexpr : vexpr
syntax:88 vexpr "!==" vexpr : vexpr
syntax:88 vexpr "=?=" vexpr : vexpr
syntax:88 vexpr "!?=" vexpr : vexpr
syntax:89 vexpr "&" vexpr : vexpr
syntax:90 vexpr "^" vexpr : vexpr
syntax:90 vexpr "^~" vexpr : vexpr
syntax:90 vexpr "~^" vexpr : vexpr
syntax:91 vexpr "|" vexpr : vexpr
syntax:92 vexpr "&&" vexpr : vexpr
syntax:93 vexpr "||" vexpr : vexpr

-- Conditional
syntax:94 vexpr "?" vexpr ":" vexpr : vexpr

-- ## vpexpr: Property / sequence expressions

syntax:98 vexpr : vpexpr
syntax:98 "(" vpexpr ")" : vpexpr
syntax:94 "not" vpexpr : vpexpr
syntax:95 vpexpr "or" vpexpr : vpexpr
syntax:95 vpexpr "and" vpexpr : vpexpr
syntax:95 vpexpr "intersect" vpexpr : vpexpr
syntax:95 vpexpr "within" vpexpr : vpexpr
syntax:96 vpexpr "|->" vpexpr : vpexpr
syntax:96 vpexpr "|=>" vpexpr : vpexpr
syntax:97 "if" "(" vexpr ")" vpexpr : vpexpr
syntax:97 "if" "(" vexpr ")" vpexpr "else" vpexpr : vpexpr

-- ## vstmt: Statements

syntax vexpr "=" vexpr ";" : vstmt
-- Nonblocking assignment: use <== instead of <= to avoid conflict with vexpr's <= operator
syntax vexpr "<==" vexpr ";" : vstmt

syntax "case" "(" vexpr ")" vcase_item* "endcase" : vstmt
syntax "casex" "(" vexpr ")" vcase_item* "endcase" : vstmt
syntax "casez" "(" vexpr ")" vcase_item* "endcase" : vstmt
syntax vexpr ":" vstmt : vcase_item
syntax "default" ":" vstmt : vcase_item

syntax "if" "(" vexpr ")" vstmt : vstmt
syntax "if" "(" vexpr ")" vstmt "else" vstmt : vstmt

syntax "forever" vstmt : vstmt
syntax "repeat" "(" vexpr ")" vstmt : vstmt
syntax "while" "(" vexpr ")" vstmt : vstmt
syntax "for" "(" vassign ";" vexpr ";" vblkassign ")" vstmt : vstmt
syntax "do" vstmt "while" "(" vexpr ")" : vstmt

syntax "return" vexpr ";" : vstmt

syntax "@" "(" vstmt_event ")" vstmt : vstmt
syntax "@" "*" vstmt : vstmt

syntax "posedge" vexpr : vstmt_event
syntax "negedge" vexpr : vstmt_event

syntax "begin" vstmt* "end" : vstmt

-- ## vassign, vblkassign, vnetdecl, vvardecl, vparamassign, vpackeddim

syntax vexpr "=" vexpr : vassign
syntax vassign "," vassign : vassign

syntax "++" ident : vblkassign
syntax ident "++" : vblkassign
syntax "dec!" ident : vblkassign
syntax ident "dec!" : vblkassign
syntax vexpr "=" vexpr : vblkassign
syntax vexpr "+=" vexpr : vblkassign
syntax vexpr "-=" vexpr : vblkassign
syntax vexpr "*=" vexpr : vblkassign
syntax vexpr "/=" vexpr : vblkassign
syntax vexpr "%=" vexpr : vblkassign
syntax vexpr "&=" vexpr : vblkassign
syntax vexpr "|=" vexpr : vblkassign
syntax vexpr "^=" vexpr : vblkassign
syntax vexpr "<<=" vexpr : vblkassign
syntax vexpr ">>=" vexpr : vblkassign
syntax vexpr "<<<=" vexpr : vblkassign
syntax vexpr ">>>=" vexpr : vblkassign

syntax ident : vnetdecl
syntax ident "=" vexpr : vnetdecl
syntax vnetdecl "," vnetdecl : vnetdecl

syntax ident : vvardecl
syntax ident vpackeddim : vvardecl
syntax vvardecl "," vvardecl : vvardecl

syntax ident "=" vexpr : vparamassign
syntax vparamassign "," vparamassign : vparamassign

syntax "[" vexpr ":" vexpr "]" : vpackeddim
syntax "[" vexpr "]" : vpackeddim
syntax vpackeddim vpackeddim : vpackeddim

-- ## vport, vparamport, vportconn

syntax ident : vport
syntax "input" ident : vport
syntax "input" "wire" ident : vport
syntax "input" "reg" ident : vport
syntax "input" "logic" ident : vport
syntax "input" vpackeddim ident : vport
syntax "input" "wire" vpackeddim ident : vport
syntax "input" "reg" vpackeddim ident : vport
syntax "input" "logic" vpackeddim ident : vport
syntax "output" ident : vport
syntax "output" "wire" ident : vport
syntax "output" "reg" ident : vport
syntax "output" "logic" ident : vport
syntax "output" vpackeddim ident : vport
syntax "output" "wire" vpackeddim ident : vport
syntax "output" "reg" vpackeddim ident : vport
syntax "output" "logic" vpackeddim ident : vport
syntax vport "," vport : vport

syntax "parameter" ident "=" vexpr : vparamport
syntax "parameter" vpackeddim ident "=" vexpr : vparamport
syntax "parameter" "integer" ident "=" vexpr : vparamport
syntax vparamport "," vparamport : vparamport

syntax "." ident : vportconnid
syntax ".*" : vportconn
syntax vportconnid : vportconn
syntax vportconnid "(" ")" : vportconn
syntax vportconnid "(" vexpr ")" : vportconn
syntax vportconn "," vportconn : vportconn

-- ## vmoditem, vgen, vtop

-- Port declarations
syntax "input" ident ";" : vmoditem
syntax "input" "wire" ident ";" : vmoditem
syntax "input" "reg" ident ";" : vmoditem
syntax "input" vpackeddim ident ";" : vmoditem
syntax "input" "wire" vpackeddim ident ";" : vmoditem
syntax "input" "reg" vpackeddim ident ";" : vmoditem
syntax "output" ident ";" : vmoditem
syntax "output" "wire" ident ";" : vmoditem
syntax "output" "reg" ident ";" : vmoditem
syntax "output" vpackeddim ident ";" : vmoditem
syntax "output" "wire" vpackeddim ident ";" : vmoditem
syntax "output" "reg" vpackeddim ident ";" : vmoditem

-- Parameter / localparam
syntax "parameter" vparamassign ";" : vmoditem
syntax "parameter" "integer" vparamassign ";" : vmoditem
syntax "localparam" vparamassign ";" : vmoditem
syntax "localparam" vpackeddim vparamassign ";" : vmoditem
syntax "localparam" "integer" vparamassign ";" : vmoditem

-- Variable declarations
syntax "bit" vvardecl ";" : vmoditem
syntax "bit" vpackeddim vvardecl ";" : vmoditem
syntax "logic" vvardecl ";" : vmoditem
syntax "logic" vpackeddim vvardecl ";" : vmoditem
syntax "reg" vvardecl ";" : vmoditem
syntax "reg" vpackeddim vvardecl ";" : vmoditem
syntax "integer" vvardecl ";" : vmoditem

-- Net declarations
syntax "wire" vnetdecl ";" : vmoditem
syntax "wire" vpackeddim vnetdecl ";" : vmoditem

-- Continuous assignment
syntax "assign" vassign ";" : vmoditem

-- Task / function
syntax "task" ident ";" vstmt "endtask" : vmoditem
syntax "function" vpackeddim ident "(" vport ")" ";" vstmt "endfunction" : vmoditem

-- Always / initial
syntax "initial" vstmt : vmoditem
syntax "always" vstmt : vmoditem
syntax "always_comb" vstmt : vmoditem
syntax "always_latch" vstmt : vmoditem
syntax "always_ff" vstmt : vmoditem

-- Module instantiation
syntax ident ident "(" vportconn ")" ";" : vmoditem

-- Generate
syntax "generate" vgen "endgenerate" : vmoditem

-- Assertions
syntax "assert" "property" "(" vpexpr ")" ";" : vmoditem
syntax "assume" "property" "(" vpexpr ")" ";" : vmoditem
syntax "cover" "property" "(" vpexpr ")" ";" : vmoditem

-- Module item sequencing
syntax vmoditem vmoditem : vmoditem

-- Generate items (note: generate-level items are module_or_generate_item, not module_item)
syntax "assign" vassign ";" : vgen
syntax "always" vstmt : vgen
syntax "always_comb" vstmt : vgen
syntax "always_latch" vstmt : vgen
syntax "always_ff" vstmt : vgen
syntax ident ident "(" vportconn ")" ";" : vgen
syntax "begin" vgen* "end" : vgen
syntax "if" "(" vexpr ")" vgen : vgen
syntax "if" "(" vexpr ")" vgen "else" vgen : vgen

-- Module declarations
syntax "module" ident "(" ")" ";" vmoditem "endmodule" : vtop
syntax "module" ident "(" vport ")" ";" vmoditem "endmodule" : vtop
syntax "module" ident "#(" vparamport ")" "(" ")" ";" vmoditem "endmodule" : vtop
syntax "module" ident "#(" vparamport ")" "(" vport ")" ";" vmoditem "endmodule" : vtop

-- Entry point
syntax "v![" vtop "]" : term

-- ## Macro rules: Elaborate syntax categories into AST terms

-- Helper: ident → String (for VId)
private def idToStr (i : Lean.Ident) : Lean.MacroM (Lean.TSyntax `term) :=
  return Lean.quote (toString i.getId)

-- ### vexpr -> term

macro_rules
  -- Unbased unsized literals
  | `(vexpr| #0) => `(expression.primary_literal (.unbased_unsized .zeros))
  | `(vexpr| #1) => `(expression.primary_literal (.unbased_unsized .ones))
  -- Decimal (no base)
  | `(vexpr| $n:num) => `(expression.primary_literal (.number (.integral (.decimal (.unsigned $n)))))
  -- Binary
  | `(vexpr| #b $v:num) => `(expression.primary_literal (.number (.integral (.binary none $v))))
  | `(vexpr| $sz:num #b $v:num) => `(expression.primary_literal (.number (.integral (.binary (some $sz) $v))))
  -- Octal
  | `(vexpr| #o $v:num) => `(expression.primary_literal (.number (.integral (.octal none $v))))
  | `(vexpr| $sz:num #o $v:num) => `(expression.primary_literal (.number (.integral (.octal (some $sz) $v))))
  -- Hex
  | `(vexpr| #h $v:num) => `(expression.primary_literal (.number (.integral (.hex none $v))))
  | `(vexpr| $sz:num #h $v:num) => `(expression.primary_literal (.number (.integral (.hex (some $sz) $v))))
  -- Decimal base
  | `(vexpr| #d $v:num) => `(expression.primary_literal (.number (.integral (.decimal (.base_unsigned none $v)))))
  | `(vexpr| $sz:num #d $v:num) => `(expression.primary_literal (.number (.integral (.decimal (.base_unsigned (some $sz) $v)))))

macro_rules
  -- Identifier
  | `(vexpr| $i:ident) => do let s ← idToStr i; `(expression.ident $s)
  -- Parenthesized
  | `(vexpr| ( $e:vexpr )) => `(vexpr| $e)

macro_rules
  -- Select / range
  | `(vexpr| $t:vexpr [ $s:vexpr ]) => `(expression.select $t $s)
  | `(vexpr| $s:vexpr [ $l:vexpr : $r:vexpr ]) => `(expression.select_const_range $s $l $r)
  | `(vexpr| $s:vexpr [ $l:vexpr +: $r:vexpr ]) => `(expression.select_indexed_range_add $s $l $r)
  | `(vexpr| $s:vexpr [ $l:vexpr -: $r:vexpr ]) => `(expression.select_indexed_range_sub $s $l $r)

macro_rules
  -- Hierarchical
  | `(vexpr| $p:vexpr · $c:vexpr) => `(expression.hierarchical_ident $p $c)

macro_rules
  -- Concatenation
  | `(vexpr| { $es:vexpr,* }) => `(expression.concat [$es,*])
  -- Multiple concatenation
  | `(vexpr| { $n:vexpr { $es:vexpr,* } }) => `(expression.mult_concat $n [$es,*])

macro_rules
  -- Cast
  | `(vexpr| $sz:vexpr #( $e:vexpr )) => `(expression.cast $sz $e)

macro_rules
  -- Function call
  | `(vexpr| $f:ident ( $args:vexpr,* )) => do
    let s ← idToStr f; `(expression.tf_call $s [$args,*])
  -- System calls
  | `(vexpr| $signed ( $e:vexpr )) => `(expression.system_tf_call .signed [$e])
  | `(vexpr| $unsigned ( $e:vexpr )) => `(expression.system_tf_call .unsigned [$e])

macro_rules
  -- Increment / decrement
  | `(vexpr| ++ $i:ident) => do let s ← idToStr i; `(expression.inc_or_dec (.inc $s))
  | `(vexpr| $i:ident ++) => do let s ← idToStr i; `(expression.inc_or_dec (.inc $s))
  | `(vexpr| dec! $i:ident) => do let s ← idToStr i; `(expression.inc_or_dec (.dec $s))
  | `(vexpr| $i:ident dec!) => do let s ← idToStr i; `(expression.inc_or_dec (.dec $s))

macro_rules
  -- Unary operators
  | `(vexpr| ﹢ $e:vexpr) => `(expression.unary_op .plus $e)
  | `(vexpr| ﹣ $e:vexpr) => `(expression.unary_op .minus $e)
  | `(vexpr| ! $e:vexpr) => `(expression.unary_op .not $e)
  | `(vexpr| ~ $e:vexpr) => `(expression.unary_op .neg $e)
  | `(vexpr| u& $e:vexpr) => `(expression.unary_op .and $e)
  | `(vexpr| u~& $e:vexpr) => `(expression.unary_op .nand $e)
  | `(vexpr| u| $e:vexpr) => `(expression.unary_op .or $e)
  | `(vexpr| u~| $e:vexpr) => `(expression.unary_op .nor $e)
  | `(vexpr| u^ $e:vexpr) => `(expression.unary_op .xor $e)
  | `(vexpr| u~^ $e:vexpr) => `(expression.unary_op .xnor $e)
  | `(vexpr| u^~ $e:vexpr) => `(expression.unary_op .xnor $e)

macro_rules
  -- Binary operators
  | `(vexpr| $l:vexpr ** $r:vexpr) => `(expression.binary_op .pow $l $r)
  | `(vexpr| $l:vexpr * $r:vexpr) => `(expression.binary_op .mul $l $r)
  | `(vexpr| $l:vexpr / $r:vexpr) => `(expression.binary_op .div $l $r)
  | `(vexpr| $l:vexpr % $r:vexpr) => `(expression.binary_op .rem $l $r)
  | `(vexpr| $l:vexpr + $r:vexpr) => `(expression.binary_op .add $l $r)
  | `(vexpr| $l:vexpr - $r:vexpr) => `(expression.binary_op .sub $l $r)
  | `(vexpr| $l:vexpr >> $r:vexpr) => `(expression.binary_op .shr $l $r)
  | `(vexpr| $l:vexpr << $r:vexpr) => `(expression.binary_op .shl $l $r)
  | `(vexpr| $l:vexpr >>> $r:vexpr) => `(expression.binary_op .sar $l $r)
  | `(vexpr| $l:vexpr <<< $r:vexpr) => `(expression.binary_op .sal $l $r)
  | `(vexpr| $l:vexpr < $r:vexpr) => `(expression.binary_op .lt $l $r)
  | `(vexpr| $l:vexpr <= $r:vexpr) => `(expression.binary_op .le $l $r)
  | `(vexpr| $l:vexpr > $r:vexpr) => `(expression.binary_op .gt $l $r)
  | `(vexpr| $l:vexpr >= $r:vexpr) => `(expression.binary_op .ge $l $r)
  | `(vexpr| $l:vexpr inside { $rs:vexpr,* }) => `(expression.inside $l [$rs,*])
  | `(vexpr| $l:vexpr == $r:vexpr) => `(expression.binary_op .eq $l $r)
  | `(vexpr| $l:vexpr != $r:vexpr) => `(expression.binary_op .neq $l $r)
  | `(vexpr| $l:vexpr === $r:vexpr) => `(expression.binary_op .feq $l $r)
  | `(vexpr| $l:vexpr !== $r:vexpr) => `(expression.binary_op .fneq $l $r)
  | `(vexpr| $l:vexpr =?= $r:vexpr) => `(expression.binary_op .weq $l $r)
  | `(vexpr| $l:vexpr !?= $r:vexpr) => `(expression.binary_op .wneq $l $r)
  | `(vexpr| $l:vexpr & $r:vexpr) => `(expression.binary_op .band $l $r)
  | `(vexpr| $l:vexpr ^ $r:vexpr) => `(expression.binary_op .bxor $l $r)
  | `(vexpr| $l:vexpr ^~ $r:vexpr) => `(expression.binary_op .bxnor $l $r)
  | `(vexpr| $l:vexpr ~^ $r:vexpr) => `(expression.binary_op .bxnor $l $r)
  | `(vexpr| $l:vexpr | $r:vexpr) => `(expression.binary_op .bor $l $r)
  | `(vexpr| $l:vexpr && $r:vexpr) => `(expression.binary_op .land $l $r)
  | `(vexpr| $l:vexpr || $r:vexpr) => `(expression.binary_op .lor $l $r)

macro_rules
  -- Conditional
  | `(vexpr| $c:vexpr ? $t:vexpr : $f:vexpr) => `(expression.cond $c $t $f)

-- ### vpexpr -> term

macro_rules
  | `(vpexpr| $e:vexpr) => `(property_expr.expr $e)
  | `(vpexpr| ( $p:vpexpr )) => `($p)
  | `(vpexpr| not $p:vpexpr) => `(property_expr.not $p)
  | `(vpexpr| $l:vpexpr or $r:vpexpr) => `(property_expr.or $l $r)
  | `(vpexpr| $l:vpexpr and $r:vpexpr) => `(property_expr.and $l $r)
  | `(vpexpr| $l:vpexpr intersect $r:vpexpr) => `(property_expr.inter $l $r)
  | `(vpexpr| $l:vpexpr within $r:vpexpr) => `(property_expr.within $l $r)
  | `(vpexpr| $l:vpexpr |-> $r:vpexpr) => `(property_expr.imp $l $r)
  | `(vpexpr| $l:vpexpr |=> $r:vpexpr) => `(property_expr.imp_n $l $r)
  | `(vpexpr| if ( $c:vexpr ) $t:vpexpr) => `(property_expr.if_else $c $t none)
  | `(vpexpr| if ( $c:vexpr ) $t:vpexpr else $f:vpexpr) => `(property_expr.if_else $c $t (some $f))

-- ### vstmt -> term

macro_rules
  | `(vstmt| $lv:vexpr = $e:vexpr ;) => `(statement_item.blocking_assign_normal $lv $e)
  | `(vstmt| $lv:vexpr <== $e:vexpr ;) => `(statement_item.nonblocking_assign $lv $e)

macro_rules
  | `(vstmt| case ( $ce:vexpr ) $cs:vcase_item* endcase) =>
    `(statement_item.case .case $ce [$cs,*])
  | `(vstmt| casex ( $ce:vexpr ) $cs:vcase_item* endcase) =>
    `(statement_item.case .casex $ce [$cs,*])
  | `(vstmt| casez ( $ce:vexpr ) $cs:vcase_item* endcase) =>
    `(statement_item.case .casez $ce [$cs,*])

macro_rules
  | `(vcase_item| $ce:vexpr : $st:vstmt) => `(case_item.case $ce $st)
  | `(vcase_item| default : $st:vstmt) => `(case_item.default $st)

macro_rules
  | `(vstmt| if ( $c:vexpr ) $t:vstmt) =>
    `(statement_item.cond $c (some $t) none)
  | `(vstmt| if ( $c:vexpr ) $t:vstmt else $f:vstmt) =>
    `(statement_item.cond $c (some $t) (some (some $f)))

macro_rules
  | `(vstmt| forever $s:vstmt) => `(statement_item.forever $s)
  | `(vstmt| repeat ( $r:vexpr ) $s:vstmt) => `(statement_item.repeat $r $s)
  | `(vstmt| while ( $w:vexpr ) $s:vstmt) => `(statement_item.while $w $s)
  | `(vstmt| for ( $init:vassign ; $ce:vexpr ; $step:vblkassign ) $s:vstmt) =>
    `(statement_item.for (for_init.var_assigns $init) $ce $step $s)
  | `(vstmt| do $s:vstmt while ( $w:vexpr )) => `(statement_item.do_while $s $w)

macro_rules
  | `(vstmt| return $e:vexpr ;) => `(statement_item.return $e)

macro_rules
  | `(vstmt_event| posedge $e:vexpr) =>
    `(event_expression.expr (some edge_identifier.posedge) $e)
  | `(vstmt_event| negedge $e:vexpr) =>
    `(event_expression.expr (some edge_identifier.negedge) $e)

macro_rules
  | `(vstmt| @ ( $ev:vstmt_event ) $s:vstmt) =>
    `(statement_item.proc_timing_control (proc_timing_control.event (event_control.expr $ev)) $s)
  | `(vstmt| @ * $s:vstmt) =>
    `(statement_item.proc_timing_control (proc_timing_control.event event_control.any) $s)

macro_rules
  | `(vstmt| begin $ss:vstmt* end) => `(statement_item.seq_block [$ss,*])

-- ### vassign -> term

macro_rules
  | `(vassign| $lv:vexpr = $e:vexpr) => `(assign.net $lv $e)
  | `(vassign| $a:vassign , $b:vassign) => `(assigns.cons $a $b)

-- ### vblkassign -> term

macro_rules
  | `(vblkassign| ++ $i:ident) => do let s ← idToStr i; `(for_step.inc_or_dec (.inc $s))
  | `(vblkassign| $i:ident ++) => do let s ← idToStr i; `(for_step.inc_or_dec (.inc $s))
  | `(vblkassign| dec! $i:ident) => do let s ← idToStr i; `(for_step.inc_or_dec (.dec $s))
  | `(vblkassign| $i:ident dec!) => do let s ← idToStr i; `(for_step.inc_or_dec (.dec $s))
  | `(vblkassign| $lv:vexpr = $e:vexpr) => `(for_step.op_assign (op_assign.op $lv .eq $e))
  | `(vblkassign| $lv:vexpr += $e:vexpr) => `(for_step.op_assign (op_assign.op $lv .add $e))
  | `(vblkassign| $lv:vexpr -= $e:vexpr) => `(for_step.op_assign (op_assign.op $lv .sub $e))
  | `(vblkassign| $lv:vexpr *= $e:vexpr) => `(for_step.op_assign (op_assign.op $lv .mul $e))
  | `(vblkassign| $lv:vexpr /= $e:vexpr) => `(for_step.op_assign (op_assign.op $lv .div $e))
  | `(vblkassign| $lv:vexpr %= $e:vexpr) => `(for_step.op_assign (op_assign.op $lv .rem $e))
  | `(vblkassign| $lv:vexpr &= $e:vexpr) => `(for_step.op_assign (op_assign.op $lv .band $e))
  | `(vblkassign| $lv:vexpr |= $e:vexpr) => `(for_step.op_assign (op_assign.op $lv .bor $e))
  | `(vblkassign| $lv:vexpr ^= $e:vexpr) => `(for_step.op_assign (op_assign.op $lv .bxor $e))
  | `(vblkassign| $lv:vexpr <<= $e:vexpr) => `(for_step.op_assign (op_assign.op $lv .shl $e))
  | `(vblkassign| $lv:vexpr >>= $e:vexpr) => `(for_step.op_assign (op_assign.op $lv .shr $e))
  | `(vblkassign| $lv:vexpr <<<= $e:vexpr) => `(for_step.op_assign (op_assign.op $lv .sal $e))
  | `(vblkassign| $lv:vexpr >>>= $e:vexpr) => `(for_step.op_assign (op_assign.op $lv .sar $e))

-- ### vnetdecl, vvardecl, vparamassign -> term

macro_rules
  | `(vnetdecl| $v:ident) => do let s ← idToStr v; `(net_decl_assign.net $s none)
  | `(vnetdecl| $v:ident = $e:vexpr) => do let s ← idToStr v; `(net_decl_assign.net $s (some $e))
  | `(vnetdecl| $a:vnetdecl , $b:vnetdecl) => `(net_decl_assigns.cons $a $b)

macro_rules
  | `(vvardecl| $v:ident) => do let s ← idToStr v; `(var_decl_assign.var $s .nil none)
  | `(vvardecl| $v:ident $d:vpackeddim) => do let s ← idToStr v; `(var_decl_assign.var $s $d none)
  | `(vvardecl| $a:vvardecl , $b:vvardecl) => `(var_decl_assigns.cons $a $b)

macro_rules
  | `(vparamassign| $p:ident = $e:vexpr) => do let s ← idToStr p; `(param_assign.param $s $e)
  | `(vparamassign| $a:vparamassign , $b:vparamassign) => `(param_assigns.cons $a $b)

-- ### vpackeddim -> term

macro_rules
  | `(vpackeddim| [ $l:vexpr : $r:vexpr ]) => `((dim.range $l $r : packed_dims))
  | `(vpackeddim| [ $d:vexpr ]) => `((dim.one $d : packed_dims))
  -- Multi-packed-dims: [a:b][c:d] → cons (first dim) (rest as packed_dims)
  | `(vpackeddim| [ $l:vexpr : $r:vexpr ] $b:vpackeddim) =>
    `(packed_dims.cons (dim.range $l $r) $b)
  | `(vpackeddim| [ $d:vexpr ] $b:vpackeddim) =>
    `(packed_dims.cons (dim.one $d) $b)

-- ### vport -> term

macro_rules
  | `(vport| $p:ident) => do
    let pid ← idToStr p
    `(ansi_port_decl.net (some (net_port_header.net none (port_type.port none .nil))) $pid)
  | `(vport| input $p:ident) => do
    let pid ← idToStr p
    `(ansi_port_decl.net (some (net_port_header.net (some .input) (port_type.port none .nil))) $pid)
  | `(vport| input wire $p:ident) => do
    let pid ← idToStr p
    `(ansi_port_decl.net (some (net_port_header.net (some .input) (port_type.port (some .wire) .nil))) $pid)
  | `(vport| input reg $p:ident) => do
    let pid ← idToStr p
    `(ansi_port_decl.var (some (var_port_header.var (some .input) (.int_vec .reg .nil))) $pid)
  | `(vport| input logic $p:ident) => do
    let pid ← idToStr p
    `(ansi_port_decl.var (some (var_port_header.var (some .input) (.int_vec .logic .nil))) $pid)
  | `(vport| input $pd:vpackeddim $p:ident) => do
    let pid ← idToStr p
    `(ansi_port_decl.net (some (net_port_header.net (some .input) (port_type.port none $pd))) $pid)
  | `(vport| input wire $pd:vpackeddim $p:ident) => do
    let pid ← idToStr p
    `(ansi_port_decl.net (some (net_port_header.net (some .input) (port_type.port (some .wire) $pd))) $pid)
  | `(vport| input reg $pd:vpackeddim $p:ident) => do
    let pid ← idToStr p
    `(ansi_port_decl.var (some (var_port_header.var (some .input) (.int_vec .reg $pd))) $pid)
  | `(vport| input logic $pd:vpackeddim $p:ident) => do
    let pid ← idToStr p
    `(ansi_port_decl.var (some (var_port_header.var (some .input) (.int_vec .logic $pd))) $pid)
  | `(vport| output $p:ident) => do
    let pid ← idToStr p
    `(ansi_port_decl.net (some (net_port_header.net (some .output) (port_type.port none .nil))) $pid)
  | `(vport| output wire $p:ident) => do
    let pid ← idToStr p
    `(ansi_port_decl.net (some (net_port_header.net (some .output) (port_type.port (some .wire) .nil))) $pid)
  | `(vport| output reg $p:ident) => do
    let pid ← idToStr p
    `(ansi_port_decl.var (some (var_port_header.var (some .output) (.int_vec .reg .nil))) $pid)
  | `(vport| output logic $p:ident) => do
    let pid ← idToStr p
    `(ansi_port_decl.var (some (var_port_header.var (some .output) (.int_vec .logic .nil))) $pid)
  | `(vport| output $pd:vpackeddim $p:ident) => do
    let pid ← idToStr p
    `(ansi_port_decl.net (some (net_port_header.net (some .output) (port_type.port none $pd))) $pid)
  | `(vport| output wire $pd:vpackeddim $p:ident) => do
    let pid ← idToStr p
    `(ansi_port_decl.net (some (net_port_header.net (some .output) (port_type.port (some .wire) $pd))) $pid)
  | `(vport| output reg $pd:vpackeddim $p:ident) => do
    let pid ← idToStr p
    `(ansi_port_decl.var (some (var_port_header.var (some .output) (.int_vec .reg $pd))) $pid)
  | `(vport| output logic $pd:vpackeddim $p:ident) => do
    let pid ← idToStr p
    `(ansi_port_decl.var (some (var_port_header.var (some .output) (.int_vec .logic $pd))) $pid)
  | `(vport| $a:vport , $b:vport) => `(ansi_port_decls.cons $a $b)

-- ### vparamport -> term

macro_rules
  | `(vparamport| parameter $p:ident = $e:vexpr) => do
    let s ← idToStr p
    `(param_decl.data (.implicit .nil) (param_assign.param $s $e))
  | `(vparamport| parameter $pd:vpackeddim $p:ident = $e:vexpr) => do
    let s ← idToStr p
    `(param_decl.data (.implicit $pd) (param_assign.param $s $e))
  | `(vparamport| parameter integer $p:ident = $e:vexpr) => do
    let s ← idToStr p
    `(param_decl.data (.data (.int_atom .integer)) (param_assign.param $s $e))
  | `(vparamport| $a:vparamport , $b:vparamport) => `(param_ports.cons $a $b)

-- ### vportconn -> term

macro_rules
  | `(vportconnid| . $i:ident) => idToStr i

macro_rules
  | `(vportconn| .*) => `(named_port_conn.wildcard)
  | `(vportconn| $pid:vportconnid) => `(named_port_conn.ident $pid)
  | `(vportconn| $pid:vportconnid ( )) => `(named_port_conn.ident $pid)
  | `(vportconn| $pid:vportconnid ( $e:vexpr )) => `(named_port_conn.expr $pid $e)
  | `(vportconn| $a:vportconn , $b:vportconn) => `(named_port_conns.cons $a $b)

-- ### vmoditem -> term

-- Port declarations
macro_rules
  | `(vmoditem| input $p:ident ;) => do
    let s ← idToStr p
    `((port_decl.input_p (.port none .nil) (.one $s) : module_item))
  | `(vmoditem| input wire $p:ident ;) => do
    let s ← idToStr p
    `((port_decl.input_p (.port (some .wire) .nil) (.one $s) : module_item))
  | `(vmoditem| input reg $p:ident ;) => do
    let s ← idToStr p
    `((port_decl.input_d (.int_vec .reg .nil) (.one $s) : module_item))
  | `(vmoditem| input $pd:vpackeddim $p:ident ;) => do
    let s ← idToStr p
    `((port_decl.input_p (.port none $pd) (.one $s) : module_item))
  | `(vmoditem| input wire $pd:vpackeddim $p:ident ;) => do
    let s ← idToStr p
    `((port_decl.input_p (.port (some .wire) $pd) (.one $s) : module_item))
  | `(vmoditem| input reg $pd:vpackeddim $p:ident ;) => do
    let s ← idToStr p
    `((port_decl.input_d (.int_vec .reg $pd) (.one $s) : module_item))
  | `(vmoditem| output $p:ident ;) => do
    let s ← idToStr p
    `((port_decl.output_p (.port none .nil) (.one $s) : module_item))
  | `(vmoditem| output wire $p:ident ;) => do
    let s ← idToStr p
    `((port_decl.output_p (.port (some .wire) .nil) (.one $s) : module_item))
  | `(vmoditem| output reg $p:ident ;) => do
    let s ← idToStr p
    `((port_decl.output_d (.int_vec .reg .nil) (.one $s) : module_item))
  | `(vmoditem| output $pd:vpackeddim $p:ident ;) => do
    let s ← idToStr p
    `((port_decl.output_p (.port none $pd) (.one $s) : module_item))
  | `(vmoditem| output wire $pd:vpackeddim $p:ident ;) => do
    let s ← idToStr p
    `((port_decl.output_p (.port (some .wire) $pd) (.one $s) : module_item))
  | `(vmoditem| output reg $pd:vpackeddim $p:ident ;) => do
    let s ← idToStr p
    `((port_decl.output_d (.int_vec .reg $pd) (.one $s) : module_item))

-- Parameter / localparam
macro_rules
  | `(vmoditem| parameter $pa:vparamassign ;) =>
    `((param_decl.data (.implicit .nil) $pa : module_item))
  | `(vmoditem| parameter integer $pa:vparamassign ;) =>
    `((param_decl.data (.data (.int_atom .integer)) $pa : module_item))
  | `(vmoditem| localparam $pa:vparamassign ;) =>
    `((local_param_decl.local (.implicit .nil) $pa : module_item))
  | `(vmoditem| localparam $pd:vpackeddim $pa:vparamassign ;) =>
    `((local_param_decl.local (.implicit $pd) $pa : module_item))
  | `(vmoditem| localparam integer $pa:vparamassign ;) =>
    `((local_param_decl.local (.data (.int_atom .integer)) $pa : module_item))

-- Variable declarations
macro_rules
  | `(vmoditem| bit $vd:vvardecl ;) =>
    `((var_decl.var (.int_vec .bit .nil) $vd : module_item))
  | `(vmoditem| bit $pd:vpackeddim $vd:vvardecl ;) =>
    `((var_decl.var (.int_vec .bit $pd) $vd : module_item))
  | `(vmoditem| logic $vd:vvardecl ;) =>
    `((var_decl.var (.int_vec .logic .nil) $vd : module_item))
  | `(vmoditem| logic $pd:vpackeddim $vd:vvardecl ;) =>
    `((var_decl.var (.int_vec .logic $pd) $vd : module_item))
  | `(vmoditem| reg $vd:vvardecl ;) =>
    `((var_decl.var (.int_vec .reg .nil) $vd : module_item))
  | `(vmoditem| reg $pd:vpackeddim $vd:vvardecl ;) =>
    `((var_decl.var (.int_vec .reg $pd) $vd : module_item))
  | `(vmoditem| integer $vd:vvardecl ;) =>
    `((var_decl.var (.int_atom .integer) $vd : module_item))

-- Net declarations
macro_rules
  | `(vmoditem| wire $nd:vnetdecl ;) =>
    `((net_decl.net .wire .nil $nd : module_item))
  | `(vmoditem| wire $pd:vpackeddim $nd:vnetdecl ;) =>
    `((net_decl.net .wire $pd $nd : module_item))

-- Continuous assignment
macro_rules
  | `(vmoditem| assign $a:vassign ;) =>
    `((cont_assign.net $a : module_item))

-- Task / function
macro_rules
  | `(vmoditem| task $tid:ident ; $st:vstmt endtask) => do
    let s ← idToStr tid
    `((task_decl.task $s (.stmt $st) : module_item))
  | `(vmoditem| function $pd:vpackeddim $fid:ident ( $ports:vport ) ; $st:vstmt endfunction) => do
    let s ← idToStr fid
    `((func_decl.func (.implicit $pd) $s $ports (.stmt $st) : module_item))

-- Always / initial
macro_rules
  | `(vmoditem| initial $st:vstmt) =>
    `((module_common_item.initial (.stmt $st) : module_item))
  | `(vmoditem| always $st:vstmt) =>
    `((module_common_item.always .always (.stmt $st) : module_item))
  | `(vmoditem| always_comb $st:vstmt) =>
    `((module_common_item.always .always_comb (.stmt $st) : module_item))
  | `(vmoditem| always_latch $st:vstmt) =>
    `((module_common_item.always .always_latch (.stmt $st) : module_item))
  | `(vmoditem| always_ff $st:vstmt) =>
    `((module_common_item.always .always_ff (.stmt $st) : module_item))

-- Module instantiation
macro_rules
  | `(vmoditem| $mid:ident $iid:ident ( $pcs:vportconn ) ;) => do
    let ms ← idToStr mid; let is ← idToStr iid
    `((module_ins.module $ms .nil (.hier $is (.named $pcs)) : module_item))

-- Generate
macro_rules
  | `(vmoditem| generate $g:vgen endgenerate) =>
    `((generated_module_ins.generated $g : module_item))

-- Assertions
macro_rules
  | `(vmoditem| assert property ( $pe:vpexpr ) ;) =>
    `((concur_assert.assert_prop (.prop none $pe) none : module_item))
  | `(vmoditem| assume property ( $pe:vpexpr ) ;) =>
    `((concur_assert.assume_prop (.prop none $pe) : module_item))
  | `(vmoditem| cover property ( $pe:vpexpr ) ;) =>
    `((concur_assert.cover_prop (.prop none $pe) none : module_item))

-- Module item sequencing
macro_rules
  | `(vmoditem| $a:vmoditem $b:vmoditem) => `(module_items.cons $a $b)

-- ### vgen -> term

macro_rules
  | `(vgen| assign $a:vassign ;) =>
    `(generate_module_item.module (.common (.cont_assign (cont_assign.net $a))))
  | `(vgen| always $st:vstmt) =>
    `(generate_module_item.module (.common (.always .always (.stmt $st))))
  | `(vgen| always_comb $st:vstmt) =>
    `(generate_module_item.module (.common (.always .always_comb (.stmt $st))))
  | `(vgen| always_latch $st:vstmt) =>
    `(generate_module_item.module (.common (.always .always_latch (.stmt $st))))
  | `(vgen| always_ff $st:vstmt) =>
    `(generate_module_item.module (.common (.always .always_ff (.stmt $st))))
  | `(vgen| $mid:ident $iid:ident ( $pcs:vportconn ) ;) => do
    let ms ← idToStr mid; let is ← idToStr iid
    `(generate_module_item.module (.ins (module_ins.module $ms .nil (.hier $is (.named $pcs)))))
  | `(vgen| begin $gs:vgen* end) => `(generate_module_item.block [$gs,*])
  | `(vgen| if ( $c:vexpr ) $t:vgen) => `(generate_module_item.cond $c $t none)
  | `(vgen| if ( $c:vexpr ) $t:vgen else $f:vgen) => `(generate_module_item.cond $c $t (some $f))

-- ### vtop -> term

macro_rules
  | `(vtop| module $n:ident ( ) ; $items:vmoditem endmodule) => do
    let s ← idToStr n; `(module_decl.ansi $s .nil .nil $items)
  | `(vtop| module $n:ident ( $ports:vport ) ; $items:vmoditem endmodule) => do
    let s ← idToStr n; `(module_decl.ansi $s .nil $ports $items)
  | `(vtop| module $n:ident #( $params:vparamport ) ( ) ; $items:vmoditem endmodule) => do
    let s ← idToStr n; `(module_decl.ansi $s $params .nil $items)
  | `(vtop| module $n:ident #( $params:vparamport ) ( $ports:vport ) ; $items:vmoditem endmodule) => do
    let s ← idToStr n; `(module_decl.ansi $s $params $ports $items)

-- ### Entry points

-- Module declaration entry point.
macro_rules
  | `(v![ $t:vtop ]) => `($t)

-- Expression entry point: parse a single Verilog expression.
syntax "vE![" vexpr "]" : term
macro_rules
  | `(vE![ $e:vexpr ]) => `($e)

-- Statement entry point: parse a single Verilog statement.
syntax "vS![" vstmt "]" : term
macro_rules
  | `(vS![ $s:vstmt ]) => `($s)

end VerilLean.Lang.Notation
