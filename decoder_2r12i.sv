`include "defs.sv"

module decoder_2r12i (
    // inputs
    input logic [31:0] instr,
    // outputs
    output logic [3:0] _2r12i_op_type
);

// Internal signals for instruction field extraction
logic [3:0] instr_extraction_31_30_28_26;
logic [1:0] instr_extraction_29_27;

always_comb begin
    // Default to invalid operation
    _2r12i_op_type = `INVALID_OP_4B;

    // Extract instruction fields for easier comparison
    instr_extraction_31_30_28_26 = {instr[31:30], instr[28], instr[26]};
    instr_extraction_29_27 = {instr[29], instr[27]};

    // Check if this is a 2R12I instruction
    if (instr_extraction_31_30_28_26 == 4'b0000) begin
        // Check arithmetic/logical instructions (instr_extraction_29_27 == 2'b00)
        if (instr_extraction_29_27 == 2'b00) begin
            if (instr[25] == 1'b1) begin
                case (instr[24:22])
                    3'b000: _2r12i_op_type = `_2R12I_SLTI;
                    3'b001: _2r12i_op_type = `_2R12I_SLTUI;
                    3'b010: _2r12i_op_type = `_2R12I_ADDI;
                    3'b101: _2r12i_op_type = `_2R12I_ANDI;
                    3'b110: _2r12i_op_type = `_2R12I_ORI;
                    3'b111: _2r12i_op_type = `_2R12I_XORI;
                    default: _2r12i_op_type = `INVALID_OP_4B;
                endcase
            end
        end
        // Check load/store instructions (instr_extraction_29_27 == 2'b11)
        else if (instr_extraction_29_27 == 2'b11) begin
            case (instr[25:22])
                4'b0000: _2r12i_op_type = `_2R12I_LD_B;   // load byte
                4'b0001: _2r12i_op_type = `_2R12I_LD_H;   // load half word
                4'b0010: _2r12i_op_type = `_2R12I_LD_W;   // load word
                4'b0100: _2r12i_op_type = `_2R12I_ST_B;   // store byte
                4'b0101: _2r12i_op_type = `_2R12I_ST_H;   // store half word
                4'b0110: _2r12i_op_type = `_2R12I_ST_W;   // store word
                4'b1000: _2r12i_op_type = `_2R12I_LD_BU;  // load byte unsigned
                4'b1001: _2r12i_op_type = `_2R12I_LD_HU;  // load half word unsigned
                4'b1011: _2r12i_op_type = `_2R12I_PRELD;  // prefetch load
                default: _2r12i_op_type = `INVALID_OP_4B;
            endcase
        end
    end
end

endmodule