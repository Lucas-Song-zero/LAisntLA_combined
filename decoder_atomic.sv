`include "defs.sv"

module decoder_atomic (
    // inputs
    input logic [31:0] instr,
    // outputs
    output logic [1:0] atomic_op_type
);

always_comb begin
    // Default to invalid operation
    atomic_op_type = `INVALID_OP_2B;
    
    // Check ATOMIC instructions based on opcode
    case (instr[31:24])
        8'b00100000: atomic_op_type = `ATOMIC_LL;
        8'b00100001: atomic_op_type = `ATOMIC_SC;
        default: atomic_op_type = `INVALID_OP_2B;
    endcase
end

endmodule