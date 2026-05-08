/- # Smoke tests for the Verilog notation system.
   Uses `#` instead of `'` for literals, `<==` for NBA, `dec!` for decrement. -/

import VerilLean.Lang.Notation

open VerilLean.Lang.Syntax
open VerilLean.Lang.Notation

-- Basic module with ports and instantiation
def tester0 : module_decl := v![
  module tester
    (input net_a,
     output net_b);
    wire net_c;
    assign net_c = net_a;
  endmodule
]

-- Parameter ports and declarations
def tester1 : module_decl := v![
  module tester ();
    parameter param_a = #0;
    parameter param_a = 4 #b 0101, param_b = #d 123;
    localparam param_a = 3 #b 111;
    localparam integer param_a = 10;
    bit net_a;
    logic net_a, net_b;
    logic [1:0] net_a;
    reg net_a;
    wire net_a;
    wire net_a = 1 #b 1;
    integer param_a;
  endmodule
]

-- Expressions: operators, select, concat
def tester2 : module_decl := v![
  module tester ();
    assign net_a = 1 #b 0;
    assign net_b = (1 + 2 #b 1) - 3 * 4 / 5;
    assign net_c = net_a[param_a];
    assign net_c = param_a ? (param_b + 1) : param_c + 1;
    assign net_a = u| {};
    assign net_a = u& {1 #b 1};
    assign net_a = {1 #b 0, 1 #b 1};
    assign net_b = {16 {1 #b 1}};
    assign net_c = net_a[0][31:0];
    assign net_c = $signed (param_a);
  endmodule
]

-- Statements: always, if/else, case, loops
def tester3 : module_decl := v![
  module tester ();
    initial
      net_a = net_b;
    always_comb
      net_a = net_b;
    always_ff @(posedge net_a) begin
      net_a <== net_c;
      net_b = net_c;
    end
    assign net_c = 1 #b 0;
    always_ff @* begin
      forever net_a = net_b;
      repeat (1 #b 1) net_a = net_b;
    end
    always @* begin
      while (1 #b 1) net_b = net_c;
      do net_c = net_a; while (1 #b 0)
    end
    always begin
      for (param_a = 0; param_a < param_b; param_a ++) begin
        net_a = net_b;
      end
    end
    always begin
      if (net_a) net_b = 1 #b 1; else begin end
    end
    always begin
      case (net_a)
        param_a: net_b <== param_b;
        default: net_c <== param_c;
      endcase
    end
  endmodule
]

-- Task, function, generate, assertions
def tester4 : module_decl := v![
  module tester ();
    task task_a;
      begin end
    endtask
    function [1:0] func_a (input [1:0] net_a);
      return net_a;
    endfunction
    assign net_b = func_a(net_a);
    generate if (net_a) begin
      assign net_a = param_a;
    end
    else begin
      assign net_b = param_b;
    end
    endgenerate
  endmodule
]

-- Assertions
def tester5 : module_decl := v![
  module tester ();
    assert property (net_a == net_b);
    assume property (param_c);
    cover property (net_a |-> net_b);
    assert property (net_a |=> net_b |-> net_c);
    assert property (net_a or net_b and net_c |-> (net_a or net_b));
  endmodule
]

-- Module with parameter ports
def tester6 : module_decl := v![
  module tester
    #(parameter [0:0] param_a = 4 #b 0010,
      parameter param_c = 3)
    (input net_a,
     input [1:0] net_c,
     output wire [31:0] net_b);
    module_a module_ins_a (.*, .net_a, .net_a(), .net_a(net_a), .*);
  endmodule
]

-- Multi-packed-dimension arrays and expression entry points
def tester7 : module_decl := v![
  module tester
    #(parameter integer ADDR_SIZE = 32,
      parameter integer DATA_SIZE = 32)
    (input logic clk,
     input logic rst_n);
    localparam integer MEM_SIZE = 2 ** (ADDR_SIZE - 2) ;
    logic [MEM_SIZE-1:0][DATA_SIZE-1:0] mem ;
  endmodule
]

-- vE![] entry point
def tester_expr : expression := vE![ a + b * c ]

-- vS![] entry point
def tester_stmt : statement := vS![ a = b + c ; ]
