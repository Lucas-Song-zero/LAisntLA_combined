`include "defs.sv"

module decoder_bar (
    // inputs
    input logic [31:0] instr,
    // outputs
    output logic [1:0] bar_op_type
);

always_comb begin
    // Default to invalid operation
    bar_op_type = `INVALID_OP_2B;
    
    // Check if this is a BAR instruction
    // Format: 0011_1000_0111_0010
    if (instr[31:16] == 16'b0011_1000_0111_0010) begin
        case (instr[15])
            1'b0: bar_op_type = `BAR_DBAR;
            1'b1: bar_op_type = `BAR_IBAR;
            default: bar_op_type = `INVALID_OP_2B;
        endcase
    end
end

endmodule
