`include "defs.sv"

module decoder_3r (
    // inputs
    input logic [31:0] instr,
    // outputs
    output logic [4:0] _3r_op_type
);

always_comb begin
    // Default to invalid operation
    _3r_op_type = `INVALID_OP_5B;
    
    // Check if this is a 3R instruction (opcode = 10'b0000_0000_00)
    if (instr[31:22] == 10'b0000_0000_00) begin
        // Check div/mod operations (instr[21] == 1)
        if (instr[21] == 1'b1) begin
            if (instr[20:17] == 4'b0000) begin
                case (instr[16:15])
                    2'b00: _3r_op_type = `_3R_DIV;
                    2'b01: _3r_op_type = `_3R_MOD;
                    2'b10: _3r_op_type = `_3R_DIVU;
                    2'b11: _3r_op_type = `_3R_MODU;
                    default: _3r_op_type = `INVALID_OP_5B;
                endcase
            end
        end
        // Check other 3R operations (instr[21] == 0)
        else begin
            if (instr[20] == 1'b1) begin
                // Check SRA, MUL operations (instr[19] == 1)
                if (instr[19] == 1'b1) begin
                    case (instr[18:15])
                        4'b0000: _3r_op_type = `_3R_SRA;
                        4'b1000: _3r_op_type = `_3R_MUL;
                        4'b1001: _3r_op_type = `_3R_MULH;
                        4'b1010: _3r_op_type = `_3R_MULHU;
                        default: _3r_op_type = `INVALID_OP_5B;
                    endcase
                end
                // Check NOR, AND, OR, XOR, SLL, SRL operations (instr[19] == 0, instr[18] == 1)
                else if (instr[18] == 1'b1) begin
                    case (instr[17:15])
                        3'b000: _3r_op_type = `_3R_NOR;
                        3'b001: _3r_op_type = `_3R_AND;
                        3'b010: _3r_op_type = `_3R_OR;
                        3'b011: _3r_op_type = `_3R_XOR;
                        3'b110: _3r_op_type = `_3R_SLL;
                        3'b111: _3r_op_type = `_3R_SRL;
                        default: _3r_op_type = `INVALID_OP_5B;
                    endcase
                end
                // Check ADD, SUB, SLT, SLTU operations (instr[19] == 0, instr[18] == 0)
                else begin
                    case (instr[17:15])
                        3'b000: _3r_op_type = `_3R_ADD;
                        3'b001: _3r_op_type = `_3R_SUB;
                        3'b100: _3r_op_type = `_3R_SLT;
                        3'b101: _3r_op_type = `_3R_SLTU;
                        default: _3r_op_type = `INVALID_OP_5B;
                    endcase
                end
            end
        end
    end
end

endmodule