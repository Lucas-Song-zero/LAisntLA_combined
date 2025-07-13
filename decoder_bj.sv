`include "defs.sv"

module decoder_bj (
    // inputs
    input logic [31:0] instr,
    // outputs
    output logic [3:0] bj_op_type
);

always_comb begin
    // Default to invalid operation
    bj_op_type = `INVALID_OP_4B;

    // Check if this is a BJ instruction (opcode = 2'b01)
    if (instr[31:30] == 2'b01) begin
        case (instr[29:26])
            4'b0011: bj_op_type = `BJ_JIRL;  // Jump and link
            4'b0100: bj_op_type = `BJ_B;     // Branch
            4'b0101: bj_op_type = `BJ_BL;    // Branch and link
            4'b0110: bj_op_type = `BJ_BEQ;   // Branch if equal
            4'b0111: bj_op_type = `BJ_BNE;   // Branch if not equal
            4'b1000: bj_op_type = `BJ_BLT;   // Branch if less than
            4'b1001: bj_op_type = `BJ_BGE;   // Branch if greater than or equal
            4'b1010: bj_op_type = `BJ_BLTU;  // Branch if less than unsigned
            4'b1011: bj_op_type = `BJ_BGEU;  // Branch if greater than or equal unsigned
            default: bj_op_type = `INVALID_OP_4B;
        endcase
    end
end

endmodule