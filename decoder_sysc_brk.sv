`include "defs.sv"

module decoder_sysc_brk (
    // inputs
    input logic [31:0] instr,
    // outputs
    output logic [1:0] sysc_brk_op_type
);

always_comb begin
    // Default to invalid operation
    sysc_brk_op_type = `INVALID_OP_2B;
    
    // Check if this is a SYSC_BRK instruction
    // Format: 0000_0000_0010_101
    if (instr[31:17] == 15'b0000_0000_0010_101) begin
        case (instr[15])
            1'b0: sysc_brk_op_type = `SB_BREAK;
            1'b1: sysc_brk_op_type = `SB_SYSCALL;
            default: sysc_brk_op_type = `INVALID_OP_2B;
        endcase
    end
end

endmodule