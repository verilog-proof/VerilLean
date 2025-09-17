namespace VerilLean.Lang.Syntax

abbrev VId := String

-- # Expressions

/-
decimal_number ::=
(v)   unsigned_number
(v) | [ size ] decimal_base unsigned_number
    | [ size ] decimal_base x_digit { _ }
    | [ size ] decimal_base z_digit { _ }
-/
inductive decimal_number where
| unsigned (v : Nat)
| base_unsigned (sz : Option Nat) (v : Nat)
deriving BEq, Inhabited, Repr

/-
integral_number ::=
(v)   decimal_number
(v) | octal_number
(v) | binary_number
(v) | hex_number
(v) binary_number ::= [ size ] binary_base binary_value
(v) octal_number ::= [ size ] octal_base octal_value
(v) hex_number ::= [ size ] hex_base hex_value
-/
inductive integral_number where
| binary (sz : Option Nat) (v : Nat)
| octal (sz : Option Nat) (v : Nat)
| hex (sz : Option Nat) (v : Nat)
| decimal (d : decimal_number)
deriving BEq, Inhabited, Repr

/- (v) number ::= integral_number | real_number -/
inductive number where
| integral (i : integral_number)
deriving BEq, Inhabited, Repr

/- unbased_unsized_literal ::= '0 | '1 | 'z_or_x -/
inductive unbased_unsized_literal where
| zeros
| ones
deriving BEq, Inhabited, Repr

/-
primary_literal ::=
(v)   number
    | time_literal
(v) | unbased_unsized_literal
    | string_literal
-/
inductive primary_literal where
| number (n : number)
| unbased_unsized (uu : unbased_unsized_literal)
deriving BEq, Inhabited, Repr

/-
(v) unary_operator ::=
+ | - | ! | ~ | & | ~& | | | ~| | ^ | ~^ | ^~
-/
inductive unary_operator where
| plus | minus | not | neg
| and | nand | or | nor | xor | xnor
deriving BEq, Inhabited, Repr

/-
(v) binary_operator ::=
  + | - | * | / | % | == | != | === | !== | =?= | !?= | && | || | **
| < |<= | > | >=| & | |  | ^  | ^~  | ~^  | >>  | <<  | >>>| <<<
-/
inductive binary_operator where
| add | sub | mul | div | rem | eq | neq | feq | fneq
| weq | wneq | land | lor | pow
| lt | le | gt | ge | band | bor | bxor | bxnor
| shr | shl | sar | sal
deriving BEq, Inhabited, Repr

/-
inc_or_dec_expression ::=
(v)   inc_or_dec_operator { attribute_instance } variable_lvalue
(v) | variable_lvalue { attribute_instance } inc_or_dec_operator
-/
inductive inc_or_dec_expression where
| inc (vid : VId)
| dec (vid : VId)
deriving BEq, Inhabited, Repr

inductive system_tf where
| signed
| unsigned
deriving BEq, Inhabited, Repr

/-
NOTE:
1) `primary` is merged into `expression`, since it has `expression` recursively.
2) L-values use expression (partially).
3) Constant expressions use expression partially as well.
-/
/-
primary ::=
(v)   primary_literal
(v) | [ implicit_class_handle . | class_scope | package_scope ] hierarchical_identifier select
(v) | empty_queue
(v) | concatenation
(v) | multiple_concatenation
(v) | function_subroutine_call
    | ( mintypmax_expression )
(v) | cast
    | streaming_expression
    | sequence_method_call
    | $
    | null
expression ::=
(v)   primary
(v) | unary_operator { attribute_instance } primary
(v) | inc_or_dec_expression
(?) | ( operator_assignment )
(v) | expression binary_operator { attribute_instance } expression
(v) | conditional_expression
(v) | inside_expression
    | tagged_union_expression
-/
inductive expression where
| primary_literal (pl : primary_literal)

/-
hierarchical_identifier ::= [ $root . ] { identifier { [ constant_expression ] } . } identifier
select ::= { [ expression ] } [ [ part_select_range ] ]
part_select_range ::= constant_range | indexed_range
indexed_range ::=
  expression +: constant_expression
| expression -: constant_expression
-/
| ident (n : VId)
| hierarchical_ident (pe ce : expression)
| select (te se : expression)
| select_const_range (se lr rr : expression)
| select_indexed_range_add (se lr rr : expression)
| select_indexed_range_sub (se lr rr : expression)

/- NOTE: maybe fine to regard (concat []) as an empty queue -/
/-
concatenation ::=
(v)   { expression { , expression } }
    | { struct_member_label : expression { , struct_member_label : expression } }
    | { array_member_label : expression { , array_member_label : expression } }
-/
| concat (es : List expression)

/- multiple_concatenation ::= { expression concatenation } -/
| mult_concat (ne : expression) (ces : List expression)

/-
function_subroutine_call ::= subroutine_call
subroutine_call ::= tf_call | (v) system_tf_call | method_call | randomize_call
system_tf_call ::= system_tf_identifier [ ( list_of_arguments ) ]
-/
| tf_call (tfid : VId) (aes : List expression)
| system_tf_call (tf : system_tf) (aes : List expression)

/-
cast ::=
(v)   casting_type ' ( expression )
    | casting_type ' concatenation
    | casting_type ' multiple_concatentation
-/
| cast (sze : expression) (e : expression)

| unary_op (op : unary_operator) (e : expression)
| inc_or_dec (iod : inc_or_dec_expression)
| binary_op (op : binary_operator) (le re : expression)
/- NOTE: need to implement patterns; see [VCondPredicate] -/
/- conditional_expression ::= cond_predicate ? { attribute_instance } expression : expression -/
| cond (ce te fe : expression)
/-
inside_expression ::= expression inside { open_range_list }
open_range_list ::= open_value_range { , open_value_range }
open_value_range ::= value_range
-/
| inside (ie : expression) (res : List expression)
deriving BEq, Inhabited, Repr

abbrev lvalue := expression

/-
net_lvalue ::=
  ps_or_hierarchical_net_identifier constant_select
| { net_lvalue { , net_lvalue } }
-/
abbrev net_lvalue := lvalue

/-
variable_lvalue ::=
  [ implicit_class_handle . | package_scope ] hierarchical_variable_identifier select
| { variable_lvalue { , variable_lvalue } }
-/
abbrev variable_lvalue := lvalue

/-
NOTE: it looks redundant to define `constant_mintypmax_expression` and
`constant_expression` separately. `expression` already contains both,
thus we use expression here.
-/
/- constant_param_expression ::= constant_mintypmax_expression | data_type | $ -/
inductive constant_param_expression where
| min_typ_max (ce : expression)
deriving BEq, Inhabited, Repr

inductive edge_identifier where
| posedge
| negedge
deriving BEq, Inhabited, Repr

/-
event_expression ::=
  [ edge_identifier ] expression [ iff expression ]
| sequence_instance [ iff expression ]
| event_expression or event_expression
| event_expression , event_expression
-/
inductive event_expression where
| expr (eid : Option edge_identifier) (e : expression)
| or (le re : event_expression)
deriving BEq, Inhabited, Repr

/-
event_control ::=
      @ hierarchical_event_identifier
(v) | @ ( event_expression )
(v) | @*
(v) | @ (*)
    | @ sequence_instance
-/
inductive event_control where
| expr (ee : event_expression)
| any
deriving BEq, Inhabited, Repr

/-
property_expr ::=
(v) | sequence_expr
(v) | ( property_expr )
(v) | not property_expr
(v) | property_expr or property_expr
(v) | property_expr and property_expr
(v) | sequence_expr |-> property_expr
(v) | sequence_expr |=> property_expr
(v) | if ( expression_or_dist ) property_expr [ else property_expr ]
    | property_instance
(v) | clocking_event property_expr
sequence_expr ::=
    | cycle_delay_range sequence_expr { cycle_delay_range sequence_expr }
    | sequence_expr cycle_delay_range sequence_expr { cycle_delay_range sequence_expr }
(v) | expression_or_dist [ boolean_abbrev ]
    | ( expression_or_dist {, sequence_match_item } ) [ boolean_abbrev ]
    | sequence_instance [ sequence_abbrev ]
    | ( sequence_expr {, sequence_match_item } ) [ sequence_abbrev ]
(v) | sequence_expr and sequence_expr
(v) | sequence_expr intersect sequence_expr
(v) | sequence_expr or sequence_expr
    | first_match ( sequence_expr {, sequence_match_item} ) | expression_or_dist throughout sequence_expr
(v) | sequence_expr within sequence_expr
(v) | clocking_event sequence_expr
-/
/- Decided to merge property_expr and sequence_expr; let's revisit if any issues occur. -/
inductive property_expr where
/- For property_expr -/
| not (se : property_expr)
| or (lse rse : property_expr)
| and (lse rse : property_expr)
| imp (lse rse : property_expr)
| imp_n (lse rse : property_expr)
| if_else (ce : expression) (tse : property_expr) (fse : Option property_expr)
| clk (ec : event_control) (se : property_expr)
/- For sequence_expr -/
| expr (e : expression)
| inter (lse rse : property_expr)
| within (lse rse : property_expr)
deriving BEq, Inhabited, Repr

-- # Assignments

/- net_assignment ::= net_lvalue = expression -/
/- variable_assignment ::= variable_lvalue = expression -/
inductive assign where
| net (lv : net_lvalue) (e : expression)
deriving BEq, Inhabited, Repr

/- list_of_net_assignments ::= net_assignment { , net_assignment } -/
/- list_of_variable_assignments ::= variable_assignment { , variable_assignment } -/
inductive assigns where
| one (na : assign)
| cons (na : assign) (nas : assigns)
deriving BEq, Inhabited, Repr

/-
assignment_operator ::=
= | += | -= | *= | /= | %= | &= | |= | ^= | <<= | >>= | <<<= | >>>=
-/
inductive assign_op where
| eq | add | sub | mul | div | rem
| band | bor | bxor | shl | shr | sal | sar
deriving BEq, Inhabited, Repr

/- operator_assignment ::= variable_lvalue assignment_operator expression -/
inductive op_assign where
| op (lv : variable_lvalue) (aop : assign_op) (e : expression)
deriving BEq, Inhabited, Repr

-- # Ports

/- port_direction ::= input | output | inout | ref -/
inductive port_direction where
| input
| output
| inout
| ref
deriving BEq, Inhabited, Repr

/- net_type ::= supply0 | supply1 | tri | triand | trior | tri0 | tri1 | wire | wand | wor -/
inductive net_type where
| wire
deriving BEq, Inhabited, Repr

/- unpacked_dimension ::= [ constant_range ] | [ constant_expression ] -/
/- packed_dimension ::= [ constant_range ] | unsized_dimension -/
/- constant_range ::= constant_expression : constant_expression -/
inductive dim where
| range (lr rr : expression)
| one (de : expression)
deriving BEq, Inhabited, Repr

inductive packed_dims where
| nil
| one (pd : dim)
| cons (pd : dim) (pds : packed_dims)
deriving BEq, Inhabited, Repr

/-
variable_dimension ::=
(v)   { sized_or_unsized_dimension }
    | associative_dimension
    | queue_dimension
sized_or_unsized_dimension ::= unpacked_dimension | unsized_dimension
-/
abbrev var_dims := packed_dims

/- integer_vector_type ::= bit | logic | reg -/
inductive int_vec_type where
| bit | logic | reg
deriving BEq, Inhabited, Repr

/- integer_atom_type ::= byte | shortint | int | longint | integer | time -/
inductive int_atom_type where
| byte | short_int | long_int | integer | time
deriving BEq, Inhabited, Repr

/-
data_type ::=
(v)   integer_vector_type [ signing ] { packed_dimension }
(v) | integer_atom_type [ signing ]
    | non_integer_type
    | struct_union [ packed [ signing ] ] { struct_union_member { struct_union_member } }
      { packed_dimension }
    | enum [ enum_base_type ] { enum_name_declaration { , enum_name_declaration } }
    | string
    | chandle
    | virtual [ interface ] interface_identifier
    | [ class_scope | package_scope ] type_identifier { packed_dimension }
    | class_type
    | event
    | ps_covergroup_identifier
-/
inductive data_type where
| int_vec (ty : int_vec_type) (pd : packed_dims)
| int_atom (ty : int_atom_type)
deriving BEq, Inhabited, Repr

/-
port_type ::=
[ net_type_or_trireg ] [ signing ] { packed_dimension }
-/
inductive port_type where
| port (nt : Option net_type) (pd : packed_dims)
deriving BEq, Inhabited, Repr

/- net_port_header ::= [ port_direction ] port_type -/
inductive net_port_header where
| net (pd : Option port_direction) (pt : port_type)
deriving BEq, Inhabited, Repr

/- variable_port_header ::= [ port_direction ] data_type -/
inductive var_port_header where
| var (pd : Option port_direction) (dt : data_type)
deriving BEq, Inhabited, Repr

/-
list_of_port_identifiers ::= port_identifier { unpacked_dimension }
{ , port_identifier { unpacked_dimension } }
-/
inductive port_ids where
| one (n : VId)
| cons (n : VId) (pis : port_ids)
deriving BEq, Inhabited, Repr

/-
port_declaration ::=
  { attribute_instance } inout_declaration
| { attribute_instance } input_declaration
| { attribute_instance } output_declaration
| { attribute_instance } ref_declaration
| { attribute_instance } interface_port_declaration
-/
inductive port_decl where
/- inout_declaration ::= inout port_type list_of_port_identifiers -/
| inout_p (pt : port_type) (pis : port_ids)
/-
input_declaration ::=
  input port_type list_of_port_identifiers
| input data_type list_of_variable_identifiers
-/
| input_p (pt : port_type) (pis : port_ids)
| input_d (dt : data_type) (pis : port_ids)
/-
output_declaration ::=
  output port_type list_of_port_identifiers
| output data_type list_of_variable_port_identifiers
-/
| output_p (pt : port_type) (pis : port_ids)
| output_d (dt : data_type) (pis : port_ids)
deriving BEq, Inhabited, Repr

/-
ansi_port_declaration ::=
(v)   [ net_port_header | interface_port_header ] port_identifier { unpacked_dimension }
(v) | [ variable_port_header ] port_identifier variable_dimension [ = constant_expression ]
    | [ net_port_header | variable_port_header ] . port_identifier ( [ expression ] )
-/
inductive ansi_port_decl where
| net (nph : Option net_port_header) (pid : VId)
| var (vph : Option var_port_header) (pid : VId)
deriving BEq, Inhabited, Repr

/-
list_of_port_declarations ::=
( [ { attribute_instance} ansi_port_declaration { , { attribute_instance} ansi_port_declaration } ] )
-/
inductive ansi_port_decls where
| nil
| one (p : ansi_port_decl)
| cons (p : ansi_port_decl) (ps : ansi_port_decls)
deriving BEq, Inhabited, Repr

-- # Statements

/- cond_predicate ::= expression_or_cond_pattern { && expression_or_cond_pattern } -/
/- expression_or_cond_pattern ::= expression | cond_pattern -/
/- cond_pattern ::= expression matches pattern -/
abbrev cond_predicate := expression

/- procedural_timing_control ::= delay_control | event_control | cycle_delay -/
inductive proc_timing_control where
| event (ec : event_control)
deriving BEq, Inhabited, Repr

/-
case_item ::=
  expression { , expression } : statement_or_null
| default [ : ] statement_or_null
-/
inductive case_item (Stmt : Type) where
| case (ce : expression) (st : Stmt)
| default (st : Stmt)
deriving BEq, Inhabited, Repr

/-
for_initialization ::=
  list_of_variable_assignments
| data_type list_of_variable_assignments { , data_type list_of_variable_assignments }
-/
inductive for_init where
| var_assigns (vas : assigns)
deriving BEq, Inhabited, Repr

/-
for_step ::= for_step_assignment { , for_step_assignment }
for_step_assignment ::= operator_assignment | inc_or_dec_expression
-/
inductive for_step where
| op_assign (oa : op_assign)
| inc_or_dec (iod : inc_or_dec_expression)
deriving BEq, Inhabited, Repr

/- case_keyword ::= case | casez | casex -/
inductive case_type where
| case | casez | casex
deriving BEq, Inhabited, Repr

/-
statement_item ::=
(v)   blocking_assignment ;
(v) | nonblocking_assignment ;
    | procedural_continuous_assignment ;
(v) | case_statement
(v) | conditional_statement
    | inc_or_dec_expression ;
    | subroutine_call_statement
    | disable_statement
    | event_trigger
(v) | loop_statement
(v) | jump_statement
    | par_block
(v) | procedural_timing_control_statement
(v) | seq_block
    | wait_statement
    | procedural_assertion_statement
    | clocking_drive ;
    | randsequence_statement
    | randcase_statement
    | expect_property_statement
-/
inductive statement_item : Type where
/-
blocking_assignment ::=
(v)   variable_lvalue = delay_or_event_control expression
    | hierarchical_dynamic_array_variable_identifier = dynamic_array_new
    | [ implicit_class_handle . | class_scope | package_scope ] hierarchical_variable_identifier
      select = class_new
    | operator_assignment
-/
| blocking_assign_normal (vlv : variable_lvalue) (e : expression)

/- nonblocking_assignment ::= variable_lvalue <= [ delay_or_event_control ] expression -/
| nonblocking_assign (vlv : variable_lvalue) (e : expression)

/-
case_statement ::=
(v)   [ unique_priority ] case_keyword ( expression ) case_item { case_item } endcase
    | [ unique_priority ] case_keyword ( expression ) matches case_pattern_item { case_pattern_item } endcase
-/
| case (cty : case_type) (ce : expression) (css : List (case_item statement_item))

/-
conditional_statement ::=
(v)   if ( cond_predicate ) statement_or_null [ else statement_or_null ]
    | unique_priority_if_statement
-/
| cond (cp : cond_predicate) (ts : Option statement_item) (fs : Option (Option statement_item))

/-
loop_statement ::=
(v)   forever statement_or_null
(v) | repeat ( expression ) statement_or_null
(v) | while ( expression ) statement_or_null
    | for ( for_initialization ; expression ; for_step ) statement_or_null
(v) | do statement_or_null while ( expression ) ;
    | foreach ( array_identifier [ loop_variables ] ) statement
-/
| forever (s : statement_item)
| repeat (re : expression) (s : statement_item)
| while (we : expression) (s : statement_item)
| for (init : for_init) (ce : expression) (step : for_step) (s : statement_item)
| do_while (s : statement_item) (we : expression)

/-
jump_statement ::=
(v)   return [ expression ] ;
    | break ;
    | continue ;
-/
| return (re : expression)

| proc_timing_control (ptc : proc_timing_control) (s : statement_item)

/-
seq_block ::=
begin [ : block_identifier ] { block_item_declaration } { statement_or_null } end [ : block_identifier ]
-/
| seq_block (ss : List statement_item)
deriving BEq, Inhabited, Repr

/- statement ::= [ block_identifier : ] { attribute_instance } statement_item -/
inductive statement where
| stmt (si : statement_item)
deriving BEq, Inhabited, Repr

-- # Module Items

/-
continuous_assign ::=
(v)  assign [ drive_strength ] [ delay3 ] list_of_net_assignments ;
    | assign [ delay_control ] list_of_variable_assignments ;
-/
inductive cont_assign where
| net (nas : assigns)
deriving BEq, Inhabited, Repr

inductive always_keyword where
| always | always_comb | always_latch | always_ff
deriving BEq, Inhabited, Repr

/- data_type_or_implicit ::= data_type | [ signing ] { packed_dimension } -/
inductive data_type_or_implicit where
| data (dt : data_type)
| implicit (pd : packed_dims)
deriving BEq, Inhabited, Repr

/-
variable_decl_assignment ::=
(v)   variable_identifier variable_dimension [ = expression ]
    | dynamic_array_variable_identifier [ ] [ = dynamic_array_new ]
    | class_variable_identifier [ = class_new ]
    | [ covergroup_variable_identifier ] = new [ ( list_of_arguments ) ]
-/
inductive var_decl_assign where
| var (vid : VId) (vd : var_dims) (ve : Option expression)
deriving BEq, Inhabited, Repr

/- list_of_variable_decl_assignments ::= variable_decl_assignment { , variable_decl_assignment } -/
inductive var_decl_assigns where
| one (pa : var_decl_assign)
| cons (pa : var_decl_assign) (pas : var_decl_assigns)
deriving BEq, Inhabited, Repr

/- variable_declaration ::= data_type list_of_variable_decl_assignments ; -/
inductive var_decl where
| var (dt : data_type) (vdas : var_decl_assigns)
deriving BEq, Inhabited, Repr

/-
data_declaration ::=
(v)   [ const ] [ lifetime ] variable_declaration
    | type_declaration
    | package_import_declaration
    | virtual_interface_declaration
-/
inductive data_decl where
| var_decl (vd : var_decl)
deriving BEq, Inhabited, Repr

/- param_assignment ::= parameter_identifier { unpacked_dimension } = constant_param_expression -/
inductive param_assign where
| param (pid : VId) (cpe : constant_param_expression)
deriving BEq, Inhabited, Repr

/- list_of_param_assignments ::= param_assignment { , param_assignment } -/
inductive param_assigns where
| one (pa : param_assign)
| cons (pa : param_assign) (pas : param_assigns)
deriving BEq, Inhabited, Repr

/-
parameter_declaration ::=
(v)   parameter data_type_or_implicit list_of_param_assignments
    | parameter type list_of_type_assignments
-/
inductive param_decl where
| data (dti : data_type_or_implicit) (pas : param_assigns)
deriving BEq, Inhabited, Repr

/-
local_parameter_declaration ::=
(v) localparam data_type_or_implicit list_of_param_assignments ;
-/
inductive local_param_decl where
| local (dti : data_type_or_implicit) (pas : param_assigns)
deriving BEq, Inhabited, Repr

/- net_decl_assignment ::= net_identifier { unpacked_dimension } [ = expression ] -/
inductive net_decl_assign where
| net (nid : VId) (ve : Option expression)
deriving BEq, Inhabited, Repr

/- list_of_net_decl_assignments ::= net_decl_assignment { , net_decl_assignment } -/
inductive net_decl_assigns where
| one (pa : net_decl_assign)
| cons (pa : net_decl_assign) (pas : net_decl_assigns)
deriving BEq, Inhabited, Repr

/-
net_declaration ::=
  net_type_or_trireg [ drive_strength | charge_strength ] [ vectored | scalared ]
  [ signing ] { packed_dimension } [ delay3 ] list_of_net_decl_assignments ;
-/
inductive net_decl where
| net (nt : net_type) (pd : packed_dims) (nda : net_decl_assigns)
deriving BEq, Inhabited, Repr

/- NOTE: tf_port_item differs a bit from ansi_port_decl, but it's still fine to reuse for certain purposes. -/
/- tf_port_list ::= tf_port_item { , tf_port_item } -/
abbrev tf_port_list := ansi_port_decls

/-
task_declaration ::= task [ lifetime ] task_body_declaration
task_body_declaration ::=
  [ interface_identifier . | class_scope ] task_identifier ;
  { tf_item_declaration }
  { statement_or_null }
  endtask [ : task_identifier ]
(v) | [ interface_identifier . | class_scope ] task_identifier ( [ tf_port_list ] ) ;
  { block_item_declaration }
  { statement_or_null }
  endtask [ : task_identifier ]
-/
inductive task_decl where
| task (tid : VId) (st : statement)
deriving BEq, Inhabited, Repr

/-
function_declaration ::= function [ lifetime ] function_body_declaration
function_body_declaration ::=
      function_data_type_or_implicit
      [ interface_identifier . | class_scope ] function_identifier ; { tf_item_declaration }
      { function_statement_or_null }
      endfunction [ : function_identifier ]
(v) | function_data_type_or_implicit
      [ interface_identifier . | class_scope ] function_identifier ( [ tf_port_list ] ) ;
      { block_item_declaration }
      { function_statement_or_null } endfunction [ : function_identifier ]
-/
inductive func_decl where
| func (dti : data_type_or_implicit) (fid : VId) (ports : tf_port_list) (st : statement)
deriving BEq, Inhabited, Repr

/-
package_or_generate_item_declaration ::=
(v)   net_declaration
(v) | data_declaration
(v) | task_declaration
(v) | function_declaration
    | dpi_import_export
    | extern_constraint_declaration
    | class_declaration
    | class_constructor_declaration
(v) | parameter_declaration ;
(v) | local_parameter_declaration
    | covergroup_declaration
    | overload_declaration
    | concurrent_assertion_item_declaration
    | ;
-/
inductive pkg_gen_item_decl where
| net (nd : net_decl)
| data (dd : data_decl)
| task (td : task_decl)
| func (fd : func_decl)
| param (pd : param_decl)
| local_param (lpd : local_param_decl)
deriving BEq, Inhabited, Repr

/-
module_or_generate_item_declaration ::=
(v)   package_or_generate_item_declaration
    | genvar_declaration
    | clocking_declaration
    | default clocking clocking_identifier ;
-/
inductive module_gen_item_decl where
| pkg (pd : pkg_gen_item_decl)
deriving BEq, Inhabited, Repr

/- NOTE: it looks like event_control includes clocking_event? -/
/- property_spec ::= [clocking_event ] [ disable iff ( expression_or_dist ) ] property_expr -/
inductive prop_spec where
| prop (oce : Option event_control) (pe : property_expr)
deriving BEq, Inhabited, Repr

/-
concurrent_assertion_item ::= [ block_identifier : ] concurrent_assertion_statement
concurrent_assertion_statement ::=
| assert_property_statement
| assume_property_statement
| cover_property_statement
assert_property_statement::= assert property ( property_spec ) action_block
action_block ::= statement_or_null | [ statement ] else statement_or_null
assume_property_statement::= assume property ( property_spec ) ;
cover_property_statement::= cover property ( property_spec ) statement_or_null
-/
inductive concur_assert where
| assert_prop (ps : prop_spec) (st : Option statement)
| assume_prop (ps : prop_spec)
| cover_prop (ps : prop_spec) (st : Option statement)
deriving BEq, Inhabited, Repr

/-
module_common_item ::=
(v)   module_or_generate_item_declaration
    | interface_instantiation
    | program_instantiation
(v) | concurrent_assertion_item
    | bind_directive
(v) | continuous_assign
    | net_alias
(v) | initial_construct
    | final_construct
(v) | always_construct
-/
inductive module_common_item where
| decl (md : module_gen_item_decl)
| assert (ca : concur_assert)
| cont_assign (a : cont_assign)
| initial (st : statement)
| always (ak : always_keyword) (st : statement)
deriving BEq, Inhabited, Repr

/- parameter_value_assignment ::= # ( list_of_parameter_assignments ) -/
inductive param_value_assigns where
| nil
deriving BEq, Inhabited, Repr

/-
named_port_connection ::=
  { attribute_instance } . port_identifier [ ( [ expression ] ) ]
(v) | { attribute_instance } .*
-/
inductive named_port_conn where
| ident (pid : VId)
| expr (pid : VId) (e : expression)
| wildcard
deriving BEq, Inhabited, Repr

inductive named_port_conns where
| one (npc : named_port_conn)
| cons (npc : named_port_conn) (npcs : named_port_conns)
deriving BEq, Inhabited, Repr

/-
list_of_port_connections ::=
  ordered_port_connection { , ordered_port_connection }
(v) | named_port_connection { , named_port_connection }
-/
inductive port_conns where
| named (npc : named_port_conns)
deriving BEq, Inhabited, Repr

/-
hierarchical_instance ::= name_of_instance ( [ list_of_port_connections ] )
name_of_instance ::= instance_identifier { unpacked_dimension }
-/
inductive hier_ins where
| hier (iid : VId) (pcs : port_conns)
deriving BEq, Inhabited, Repr

/- NOTE: currently only supports a single instantiation -/
/-
module_instantiation ::=
module_identifier [ parameter_value_assignment ] hierarchical_instance { , hierarchical_instance } ;
-/
inductive module_ins where
| module (mid : VId) (pva : param_value_assigns) (mins : hier_ins)
deriving BEq, Inhabited, Repr

/-
module_or_generate_item ::=
  { attribute_instance } parameter_override
| { attribute_instance } gate_instantiation
| { attribute_instance } udp_instantiation
(v) | { attribute_instance } module_instantiation
(v) | { attribute_instance } module_common_item
-/
inductive module_or_generate_item where
| ins (mi : module_ins)
| common (ci : module_common_item)
deriving BEq, Inhabited, Repr

/-
generate_module_item ::=
(v)   generate_module_conditional_statement
    | generate_module_case_statement
    | generate_module_loop_statement
(v) | [ generate_block_identifier : ] generate_module_block
(v) | module_or_generate_item
generate_module_conditional_statement ::=
  if ( constant_expression ) generate_module_item [ else generate_module_item ]
generate_module_block ::=
  begin [ : generate_block_identifier ] { generate_module_item } end [ : generate_block_identifier ]
-/
inductive generate_module_item where
| cond (ce : expression) (tgmi : generate_module_item) (fgmi : Option generate_module_item)
| block (gmi : List generate_module_item)
| module (mgi : module_or_generate_item)
deriving BEq, Inhabited, Repr

/- generated_module_instantiation ::= generate { generate_module_item } endgenerate -/
inductive generated_module_ins where
| generated (gmi : generate_module_item)
deriving BEq, Inhabited, Repr

/-
non_port_module_item ::=
(v)   generated_module_instantiation
(v) | module_or_generate_item
    | specify_block
    | { attribute_instance } specparam_declaration
    | program_declaration
    | module_declaration
    | timeunits_declaration
-/
inductive non_port_module_item where
| generated_module_ins (gmi : generated_module_ins)
| module_or_generate_item (mog : module_or_generate_item)
deriving BEq, Inhabited, Repr

inductive module_item where
| port_decl (pd : port_decl)
| non_port (np : non_port_module_item)
deriving BEq, Inhabited, Repr

/-
parameter_port_declaration ::=
(v)   parameter_declaration
    | data_type list_of_param_assignments
    | type list_of_type_assignments
parameter_port_list ::=
(v)   # ( list_of_param_assignments { , parameter_port_declaration } )
(v) | # ( parameter_port_declaration { , parameter_port_declaration } )
-/
inductive param_ports where
| nil
| one (pd : param_decl)
| cons (pd : param_decl) (pds : param_ports)
deriving BEq, Inhabited, Repr

inductive module_items where
| one (i : module_item)
| cons (i : module_item) (is : module_items)
deriving BEq, Inhabited, Repr

/-
module_declaration ::=
      module_nonansi_header [ timeunits_declaration ] { module_item }
      endmodule [ : module_identifier ]
(v) | module_ansi_header [ timeunits_declaration ] { non_port_module_item }
      endmodule [ : module_identifier ]
    | { attribute_instance } module_keyword [ lifetime ] module_identifier ( .* ) ;
      [ timeunits_declaration ] { module_item } endmodule [ : module_identifier ]
    | extern module_nonansi_header
    | extern module_ansi_header
module_ansi_header ::=
  { attribute_instance } module_keyword [ lifetime ] module_identifier [ parameter_port_list ]
  [ list_of_port_declarations ] ;
-/
inductive module_decl where
| ansi (name : VId) (params : param_ports) (ports : ansi_port_decls) (items : module_items)
deriving BEq, Inhabited, Repr

end VerilLean.Lang.Syntax
