`include "defs.sv"

module decoder_1r20i (
    // inputs
    input logic [31:0] instr,
    // outputs
    output logic [1:0] _1r20i_op_type
);

// Internal signal for instruction field extraction
logic [5:0] instr_extraction_31_to_28_26_25;

always_comb begin
    // Default to invalid operation
    _1r20i_op_type = `INVALID_OP_2B;

    // Extract instruction fields for easier comparison
    // Format: instr[31:28] + instr[26] + instr[25]
    instr_extraction_31_to_28_26_25 = {instr[31:28], instr[26], instr[25]};

    // Check if this is a 1R20I instruction
    // Format: 0000_10 (LU12I and PCADDU12I)
    if (instr_extraction_31_to_28_26_25 == 6'b0000_10) begin
        case (instr[27])
            1'b0: _1r20i_op_type = `_1R20I_LU12I;      // Load Upper 12-bit Immediate
            1'b1: _1r20i_op_type = `_1R20I_PCADDU12I;  // PC Add Upper 12-bit Immediate
            default: _1r20i_op_type = `INVALID_OP_2B;
        endcase
    end
end

endmodule