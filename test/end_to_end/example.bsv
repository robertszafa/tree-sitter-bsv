package Top;
import BRAM::*;
import FIFO::*;
import StmtFSM::*;
import GetPut::*;
import Vector::*;

Integer kN = 1000;

// BRAMRequest#(int, int) BRAMRequest makeRequest 
// makeRequest(Bool write, int addr, int data);
// function BRAMRequest#(int, int) makeRequest(Bool write, int addr, int data);
function BRAMRequest#(int, int) makeRequest(Bool write, int addr, int data);
  return BRAMRequest{
    write: write,
    responseOnWrite: False,
    address: addr,
    datain: data
  };
endfunction


typedef union tagged {
  bit [4:0] Register;
  bit [21:0] Literal;
  struct {
    bit [4:0] regAddr;
    bit [4:0] regIndex;
  } Indexed;
} InstrOperand;



typedef enum
  { R0 , R1 , R2 , R3 , R4 , R5 , R6 , R7 ,
    R8 , R9 , R10, R11, R12, R13, R14, R15,
    R16, R17, R18, R19, R20, R21, R22, R23,
    R24, R25, R26, R27, R28, R29, R30, R31 } RName deriving (Bits, Eq);
  // typedef Bit#(5) RName;
  // Note: assume R0 reads 0, discards writes

typedef RName Src;
typedef RName Dest;
typedef RName Cond;
typedef RName Addr;
typedef RName Val;

typedef union tagged {
    struct { Dest rd; Src   ra; Src rb; } Add;
    struct { Cond cd; Addr  addr; }       Jz;
    struct { Dest rd; Addr  addr; }       Load;
    struct { Val  v;  Addr  addr; }       Store;
    void                                  Halt;
} Instr deriving (Bits);

function Instr nextInstr(int n);
   case (n)
      3 :  return (tagged Add {rd:R3, ra:R0, rb:R1});
      4 :  return (tagged Add {rd:R4, ra:R3, rb:R2});
      5 :  return (tagged Store {v:R4, addr:R1});
      7 :  return (tagged Jz {cd:R0, addr:R5});
      8 :  return (tagged Add {rd:R4, ra:R4, rb:R4});
      9 :  return (tagged Store {v:R4, addr:R0});
     10 :  return (tagged Halt);
   endcase
endfunction: nextInstr


interface StartStop_ifc;
  method Action put_args(int n);
  method Bool done;
endinterface


(* synthesize *)
module mkDut(StartStop_ifc);
  // Dual port BRAM
  BRAM_Configure cfg = defaultValue;
  cfg.memorySize = kN+1; 
  BRAM2Port#(int, int) b_A <- mkBRAM2Server(cfg);

  // Registers
  Reg#(int) r_n <- mkRegU; 
  Reg#(Bool) r_start <- mkReg(False);
  Reg#(Bool) r_done <- mkReg(False);

  // Body registers
  Reg#(int) r_i <- mkReg(0);

  FIFO#(int) f_n <- mkSizedFIFO(4);
  FIFO#(int) f_i <- mkSizedFIFO(4);
  FIFO#(int) f_i2 <- mkSizedFIFO(4);
  FIFO#(int) f_i3 <- mkSizedFIFO(4);
  FIFO#(int) f_i4 <- mkSizedFIFO(4);
  FIFO#(int) f_a <- mkSizedFIFO(4);

  /// Rules
// ----------------------------------------------------------------------------
  rule rl_loop;
    f_i.enq(r_i);
  endrule
// ----------------------------------------------------------------------------
  rule put_ld_req; 
    let i = f_i.first;
    f_i.deq;

    let req_ld = makeRequest(False, i, ?);
    b_A.portA.request.put(req_ld);
  endrule
// ----------------------------------------------------------------------------
  rule get_ld_req; 
    let a <- b_A.portA.response.get;
    f_a.enq(a);
  endrule
// ----------------------------------------------------------------------------
  rule body1 (f_a.first > 0); 
    let a = f_a.first;
    let i = f_i2.first;

    let new_a = a+1;
    let req_st = makeRequest(True, i, new_a);
    b_A.portB.request.put(req_st);

    f_a.deq;
    f_i2.deq;
  endrule
// ----------------------------------------------------------------------------
  rule body2 (f_a.first <= 0);  
    f_a.deq;
    f_i2.deq;
  endrule
// ----------------------------------------------------------------------------
  rule exit (r_start && r_i >= r_n);  
    // r_done <= True;
    // r_start <= False;
  endrule
// ----------------------------------------------------------------------------

  /// Interface
  method Action put_args (int n) if (!r_start); 
    r_n <= n;
    r_start <= True;
  endmethod
  method Bool done;
    return r_done; 
  endmethod

endmodule


(* synthesize *)
module mkTop();
  Reg#(Bool) r_start <- mkReg(True); 
  Reg#(int) r_cycle <- mkReg (0);

  StartStop_ifc dut <- mkDut;

  InstrOperand orand;
  orand = tagged Indexed { regAddr:3, regIndex:4 };

  rule rl_count_cycles;
    r_cycle <= r_cycle + 1;
  endrule

  rule rl_start (r_start);
    dut.put_args(fromInteger(kN));
    r_start <= False;
  endrule

  rule rl_end (dut.done);
    $display("Num cycles = %d", r_cycle);
    $finish;
  endrule

endmodule

endpackage

