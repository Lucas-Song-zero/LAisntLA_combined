`include "defs.sv"

module decoder_2r5i (
    // inputs
    input logic [31:0] instr,
    // outputs
    output logic [1:0] _2r5i_op_type
);

always_comb begin
    // Default to invalid operation
    _2r5i_op_type = `INVALID_OP_2B;

    // Check if this is a 2R5I instruction
    // Format: 0000_0000_0100_xxxx_001 (SLLI, SRLI, SRAI)
    if (instr[31:20] == 12'b0000_0000_0100 && instr[17:15] == 3'b001) begin
        case (instr[19:18])
            2'b00: _2r5i_op_type = `_2R5I_SLLI;  // Shift left logical immediate
            2'b01: _2r5i_op_type = `_2R5I_SRLI;  // Shift right logical immediate
            2'b10: _2r5i_op_type = `_2R5I_SRAI;  // Shift right arithmetic immediate
            default: _2r5i_op_type = `INVALID_OP_2B;
        endcase
    end
end

endmodule