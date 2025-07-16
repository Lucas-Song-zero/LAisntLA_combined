`include "defs.sv"
module simple_fu(
    input logic clk,
    input logic rst_n,

    // interface with simple_IQ
    input simple_issue_queue_issued_info_t issued_info,
    input logic issue_valid,
    output logic fu_ready,

    // read from PRF or imm or bypass
    output logic [`PREG_INDEX_WIDTH-1:0] prf_read_rj_index,
    output logic [`PREG_INDEX_WIDTH-1:0] prf_read_rk_index,
    input logic [31:0] rj_val,
    input logic [31:0] rk_val,

    // alu output
    output logic [31:0] result,
    output logic [`PREG_INDEX_WIDTH-1:0] wb_rd_index, // 如果rd存在就写回rd，如果不存在就写回 rd=0
    output logic bj_taken,
    output logic [1:0] real_cut_pos,
    output logic pred_wrong,
    output logic [`ROB_MAINBODY_ENTRY_exception_width-1:0] exception,
    output logic result_valid,
    output logic [`ROB_ENTRY_INDEX_WIDTH-1:0] wb_rob_entry_index, // 写回ROB的寻址entry index
    output logic preg_rd_exist
);

// after one cycle delay, pass the preg rd index to the output
always@ (posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        wb_rd_index <= 0;
    end else begin
        wb_rd_index <= issued_info.issued_preg_rd;
    end
end

// after one cycle delay, pass the rob_entry_index to the output
always@ (posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        wb_rob_entry_index <= 0;
    end else begin
        wb_rob_entry_index <= issued_info.issued_rob_entry_index;
    end
end


// in this simple_fu decide below variables
logic [31:0] operand_a;
logic [31:0] operand_b;
logic addsub_mode_sel; // high is add, low is sub
logic [31:0] addsub_result;
logic [63:0] mul_result_signed; // 64bits output
logic [63:0] mul_result_unsigned; // 64bits output
logic [3:0] result_sel; // final bitwidth needs check (temp 4bits)
/*
    0 -- addsub_result
    1 --mul_res_signed_low32
    10 -- mul_res_signed_high32
    11 -- mul_res_unsigned_low32
    100 -- mul_res_unsigned_high32
    101 --shift_result
    110 -- compare_result
    111 -- logic_result
    1000 -- bj_result
*/
// first init FUs
sign_addsub u_sign_addsub (
    .clk(clk),
    .A(operand_a),
    .B(operand_b),
    .S(addsub_result),
    .CE(1'b1),
    .ADD(addsub_mode_sel)
);

sign_mul u_sign_mul (
    .clk(clk),
    .A(operand_a),
    .B(operand_b),
    .P(mul_result_signed)
);

unsign_mul u_unsign_mul (
    .clk(clk),
    .A(operand_a),
    .B(operand_b),
    .P(mul_result_unsigned)
);
// ip-customized FUs are all above, other simple ops should also cost 1 cycle to get synchronized

logic [31:0] shift_result;
logic [31:0] compare_result;
logic [31:0] logic_result;
logic [31:0] bj_result;

// always@ shift_result
always@ (posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        shift_result <= 32'b0;
    end else     if(issued_info.issued_gen_op_type == `GENERAL_OPTYPE_3R || issued_info.issued_gen_op_type == `GENERAL_OPTYPE_2R5I || issued_info.issued_gen_op_type == `GENERAL_OPTYPE_1R20I) begin
        case (issued_info.issued_spec_op_type)
            `_3R_SLL: shift_result <= rj_val << rk_val[4:0];
            `_3R_SRL: shift_result <= rj_val >> rk_val[4:0];
            `_3R_SRA: shift_result <= $signed(rj_val) >>> rk_val[4:0];
            `_2R5I_SLLI: shift_result <= rj_val << issued_info.issued_imm[4:0];
            `_2R5I_SRLI: shift_result <= rj_val >> issued_info.issued_imm[4:0];
            `_2R5I_SRAI: shift_result <= $signed(rj_val) >>> issued_info.issued_imm[4:0];
            `_1R20I_LU12I: shift_result <= {issued_info.issued_imm[19:0], 12'b0};
            `_1R20I_PCADDU12I: shift_result <= issued_info.issued_pc + {issued_info.issued_imm[19:0], 12'b0};
            default: shift_result <= 32'b0;
        endcase
    end
end

// always@ compare_result
always@ (posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        compare_result <= 32'b0;
    end else if(issued_info.issued_gen_op_type == `GENERAL_OPTYPE_3R || issued_info.issued_gen_op_type == `GENERAL_OPTYPE_2R12I) begin
        case (issued_info.issued_spec_op_type)
            `_3R_SLT: compare_result <= $signed(rj_val) < $signed(rk_val);
            `_3R_SLTU: compare_result <= rj_val < rk_val;
            `_2R12I_SLTI: compare_result <= $signed(rj_val) < $signed({20{issued_info.issued_imm[11]}, issued_info.issued_imm[11:0]});
            `_2R12I_SLTUI: compare_result <= rj_val < {20{issued_info.issued_imm[11]}, issued_info.issued_imm[11:0]};
            default: compare_result <= 32'b0;
        endcase
    end
end

// always@ logic_result
always@ (posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        logic_result <= 32'b0;
    end else if(issued_info.issued_gen_op_type == `GENERAL_OPTYPE_3R || issued_info.issued_gen_op_type == `GENERAL_OPTYPE_2R12I) begin
        case (issued_info.issued_spec_op_type)
            `_3R_NOR: logic_result <= ~(rj_val | rk_val);
            `_3R_AND: logic_result <= rj_val & rk_val;
            `_3R_OR: logic_result <= rj_val | rk_val;
            `_3R_XOR: logic_result <= rj_val ^ rk_val;
            `_2R12I_ANDI: logic_result <= rj_val & {20'b0, issued_info.issued_imm[11:0]}; // zero-extend imm_12
            `_2R12I_ORI: logic_result <= rj_val | {20'b0, issued_info.issued_imm[11:0]}; // zero-extend imm_12
            `_2R12I_XORI: logic_result <= rj_val ^ {20'b0, issued_info.issued_imm[11:0]}; // zero-extend imm_12
            default: logic_result <= 32'b0;
        endcase
    end
end

// always@ bj_result
always@ (posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        bj_result <= 32'b0;
    end else if(issued_info.issued_gen_op_type == `GENERAL_OPTYPE_BJ) begin
        case (issued_info.issued_spec_op_type)
            `BJ_BEQ: begin
                bj_taken = rj_val == rk_val;
                bj_result = issued_info.issued_pc + {14{issued_info.issued_imm[15]}, issued_info.issued_imm[15:0], 2'b0}; // sign-extend offs16
                pred_wrong = (bj_taken != issued_info.issued_pred_taken) || (issued_info.issued_pred_jump_pc != bj_result);
            end
            `BJ_BNE: begin
                bj_taken = rj_val != rk_val;
                bj_result = issued_info.issued_pc + {14{issued_info.issued_imm[15]}, issued_info.issued_imm[15:0], 2'b0}; // sign-extend offs16
                pred_wrong = (bj_taken != issued_info.issued_pred_taken) || (issued_info.issued_pred_jump_pc != bj_result);
            end
            `BJ_BLT: begin
                bj_taken = $signed(rj_val) < $signed(rk_val);
                bj_result = issued_info.issued_pc + {14{issued_info.issued_imm[15]}, issued_info.issued_imm[15:0], 2'b0}; // sign-extend offs16
                pred_wrong = (bj_taken != issued_info.issued_pred_taken) || (issued_info.issued_pred_jump_pc != bj_result);
            end
            `BJ_BGE: begin
                bj_taken = $signed(rj_val) >= $signed(rk_val);
                bj_result = issued_info.issued_pc + {14{issued_info.issued_imm[15]}, issued_info.issued_imm[15:0], 2'b0}; // sign-extend offs16
                pred_wrong = (bj_taken != issued_info.issued_pred_taken) || (issued_info.issued_pred_jump_pc != bj_result);
            end
            `BJ_BLTU: begin
                bj_taken = rj_val < rk_val; // unsigned comparison
                bj_result = issued_info.issued_pc + {14{issued_info.issued_imm[15]}, issued_info.issued_imm[15:0], 2'b0}; // sign-extend offs16
                pred_wrong = (bj_taken != issued_info.issued_pred_taken) || (issued_info.issued_pred_jump_pc != bj_result);
            end
            `BJ_BGEU: begin
                bj_taken = rj_val >= rk_val; // unsigned comparison
                bj_result = issued_info.issued_pc + {14{issued_info.issued_imm[15]}, issued_info.issued_imm[15:0], 2'b0}; // sign-extend offs16
                pred_wrong = (bj_taken != issued_info.issued_pred_taken) || (issued_info.issued_pred_jump_pc != bj_result);
            end
            `BJ_JIRL: begin
                bj_taken = 1'b1;
                bj_result = issued_info.issued_pc + {14{issued_info.issued_imm[15]}, issued_info.issued_imm[15:0], 2'b0}; // sign-extend offs16
                pred_wrong = (bj_taken != issued_info.issued_pred_taken) || (issued_info.issued_pred_jump_pc != bj_result);
            end
            `BJ_B: begin
                bj_taken = 1'b1;
                bj_result = {4{issued_info.issued_imm[25]},issued_info.issued_imm[25:0],2'b0}; // sign-extend offs26
                pred_wrong = (bj_taken != issued_info.issued_pred_taken) || (issued_info.issued_pred_jump_pc != bj_result);
            end
            `BJ_BL: begin
                bj_taken = 1'b1;
                bj_result = {4{issued_info.issued_imm[25]},issued_info.issued_imm[25:0],2'b0}; // sign-extend offs26
                pred_wrong = (bj_taken != issued_info.issued_pred_taken) || (issued_info.issued_pred_jump_pc != bj_result);
            end
            default: begin
                bj_taken = 1'b0;
                pred_wrong = 1'b0;
                bj_result = 32'b0;
            end
        endcase
    end
end

// always@ result_valid
always@ (posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        result_valid <= 1'b0;
    end else if (issued_info.issued_gen_op_type != `INVALID_OP_4B && IQ_valid == 1'b1) begin
        result_valid <= 1'b1;
    end else begin
        result_valid <= 1'b0;
    end
end

// always_comb FU_ready (always ready)
always_comb begin
    FU_ready = 1'b1; // because all the ops exec in 1 cycle
end

logic [31:0] mul_result_signed_low32;
logic [31:0] mul_result_signed_high32;
logic [31:0] mul_result_unsigned_low32;
logic [31:0] mul_result_unsigned_high32;
// mul_result_signed split
always_comb begin
    mul_result_signed_low32 = mul_result_signed[31:0];
    mul_result_signed_high32 = mul_result_signed[63:32];
end

// mul_result_unsigned split
always_comb begin
    mul_result_unsigned_low32 = mul_result_unsigned[31:0];
    mul_result_unsigned_high32 = mul_result_unsigned[63:32];
end

always_comb begin // decide operand_a and operand_b and addsub_mode_sel etc.

    // default settings
    operand_a = 32'b0;
    operand_b = 32'b0;
    addsub_mode_sel = 1'b0;
    result_sel = 4'b0;

    if(issued_info.issued_gen_op_type == `GENERAL_OPTYPE_3R) begin
        case (issued_info.issued_spec_op_type)
            `_3R_ADD: begin
                operand_a = rj_val;
                operand_b = rk_val;
                addsub_mode_sel = 1'b1; // high is add, low is sub
                result_sel = `ADDSUB_RES_SEL; // addsub_result
            end
            `_3R_SUB: begin
                operand_a = rj_val;
                operand_b = rk_val;
                addsub_mode_sel = 1'b0;
                result_sel = `ADDSUB_RES_SEL; // addsub_result
            end
            `_3R_SLT: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `COMPARE_RES_SEL; // compare_result
            end
            `_3R_SLTU: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `COMPARE_RES_SEL; // compare_result
            end
            `_3R_NOR: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `LOGIC_RES_SEL; // logic_result
            end
            `_3R_AND: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `LOGIC_RES_SEL; // logic_result
            end
            `_3R_OR: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `LOGIC_RES_SEL; // logic_result
            end
            `_3R_XOR: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `LOGIC_RES_SEL; // logic_result
            end
            `_3R_SLL: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `SHIFT_RES_SEL; // shift_result
            end
            `_3R_SRL: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `SHIFT_RES_SEL; // shift_result
            end
            `_3R_SRA: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `SHIFT_RES_SEL; // shift_result
            end
            `_3R_MUL: begin
                operand_a = rj_val;
                operand_b = rk_val;
                result_sel = `MUL_RES_SIGNED_LOW32_SEL; // mul_result_low32
            end
            `_3R_MULH: begin
                operand_a = rj_val;
                operand_b = rk_val;
                result_sel = `MUL_RES_SIGNED_HIGH32_SEL; // mul_result_high32
            end
            `_3R_MULHU: begin
                operand_a = rj_val;
                operand_b = rk_val;
                result_sel = `MUL_RES_UNSIGNED_HIGH32_SEL; // mul_result_low32
            end
        endcase
    end else if(issued_info.issued_gen_op_type == `GENERAL_OPTYPE_2R12I) begin
        case (issued_info.issued_spec_op_type)
            `_2R12I_SLTI: begin
                result_sel = `COMPARE_RES_SEL; // compare_result
            end
            `_2R12I_SLTUI: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `COMPARE_RES_SEL; // compare_result
            end
            `_2R12I_ADDI: begin
                operand_a = rj_val;
                operand_b = {20{issued_info.issued_imm[11]}, issued_info.issued_imm[11:0]}; // sign-extend
                result_sel = `ADDSUB_RES_SEL; // addsub_result
            end
            `_2R12I_ANDI: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `LOGIC_RES_SEL; // logic_result
            end
            `_2R12I_ORI: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `LOGIC_RES_SEL; // logic_result
            end
            `_2R12I_XORI: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `LOGIC_RES_SEL; // logic_result
            end
            // other 2R12I instrs are LS instr, not included here
        endcase
    end else if(issued_info.issued_gen_op_type == `GENERAL_OPTYPE_BJ) begin
        case (issued_info.issued_spec_op_type)
            `BJ_BEQ: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `BJ_RES_SEL; // bj_result
            end
            `BJ_BNE: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `BJ_RES_SEL; // bj_result
            end
            `BJ_BLT: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `BJ_RES_SEL; // bj_result
            end
            `BJ_BGE: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `BJ_RES_SEL; // bj_result
            end
            `BJ_BLTU: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `BJ_RES_SEL; // bj_result
            end
            `BJ_BGEU: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `BJ_RES_SEL; // bj_result
            end
            `BJ_JIRL: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `BJ_RES_SEL; // bj_result
            end
            `BJ_B: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `BJ_RES_SEL; // bj_result
            end
            `BJ_BL: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `BJ_RES_SEL; // bj_result
            end
        endcase
    end else if(issued_info.issued_gen_op_type == `GENERAL_OPTYPE_2R5I) begin
        case (issued_info.issued_spec_op_type)
            `_2R5I_SLLI: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `SHIFT_RES_SEL; // shift_result
            end
            `_2R5I_SRLI: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `SHIFT_RES_SEL; // shift_result
            end
            `_2R5I_SRAI: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `SHIFT_RES_SEL; // shift_result
            end
        endcase
    end else if(issued_info.issued_gen_op_type == `GENERAL_OPTYPE_1R20I) begin
        case (issued_info.issued_spec_op_type)
            `_1R20I_LU12I: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `SHIFT_RES_SEL; // shift_result
            end
            `_1R20I_PCADDU12I: begin
                // below in always @ block (no use of operand_a and operand_b)
                result_sel = `SHIFT_RES_SEL; // bj_result
            end
        endcase
    end
end

always_comb begin // decide result by result_sel
    case (result_sel)
        `ADDSUB_RES_SEL: result = addsub_result;
        `MUL_RES_SIGNED_LOW32_SEL: result = mul_result_signed_low32;
        `MUL_RES_SIGNED_HIGH32_SEL: result = mul_result_signed_high32;
        `MUL_RES_UNSIGNED_LOW32_SEL: result = mul_result_unsigned_low32;
        `MUL_RES_UNSIGNED_HIGH32_SEL: result = mul_result_unsigned_high32;
        `SHIFT_RES_SEL: result = shift_result;
        `COMPARE_RES_SEL: result = compare_result;
        `LOGIC_RES_SEL: result = logic_result;
        `BJ_RES_SEL: result = bj_result;
        default: result = 32'b0;
    endcase
end
endmodule