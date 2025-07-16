`include "defs.sv"

module complex_fu (
    input logic clk,
    input logic rst_n,

    // interface with complex_IQ
    // verified that the IP core--div can pipeline output
    input complex_issue_queue_issued_info_t issued_info,
    input logic issue_valid,
    output logic fu_ready,

    // read from PRF or imm or bypass
    output logic [`PREG_INDEX_WIDTH-1:0] prf_read_rj_index,
    output logic [`PREG_INDEX_WIDTH-1:0] prf_read_rk_index,
    input logic [31:0] rj_val,
    input logic [31:0] rk_val,

    // write to PRF
    output logic [31:0] result,
    output logic [`PREG_INDEX_WIDTH-1:0] wb_rd_index,
    output logic [`ROB_ENTRY_INDEX_WIDTH-1:0] wb_rob_entry_index,
    output logic result_valid,
    output logic preg_rd_exist
);

// 流水线寄存器，用于存储指令信息
// 修改这里的数组大小可以改变延迟周期数
typedef struct packed {
    logic [`ROB_ENTRY_INDEX_WIDTH-1:0] rob_entry_index;
    logic [`PREG_INDEX_WIDTH-1:0] preg_rd_index;
    logic [3:0] gen_op_type;
    logic [4:0] spec_op_type;
    logic valid;
} pipeline_stage_t;

pipeline_stage_t pipeline_regs [3:0]; // 4级流水线寄存器
// 后面根据DIV/MOD的执行周期数 会修改这个数组的大小

// 流水线寄存器更新逻辑
always@ (posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        for(int i = 0; i < 4; i++) begin
            pipeline_regs[i] <= '0;
        end
    end else begin
        // 流水线推进
        for(int i = 3; i > 0; i--) begin
            pipeline_regs[i] <= pipeline_regs[i-1];
        end
        // 第一级接收新指令
        pipeline_regs[0].rob_entry_index <= issued_info.issued_rob_entry_index;
        pipeline_regs[0].preg_rd_index <= issued_info.issued_preg_rd;
        pipeline_regs[0].gen_op_type <= issued_info.issued_gen_op_type;
        pipeline_regs[0].spec_op_type <= issued_info.issued_spec_op_type;
        pipeline_regs[0].valid <= issue_valid;
    end
end

// 4个周期延迟后输出有效信号
assign result_valid = pipeline_regs[3].valid;

// 4个周期延迟后输出ROB entry index
assign wb_rob_entry_index = pipeline_regs[3].rob_entry_index;

// 4个周期延迟后输出物理寄存器索引
assign wb_rd_index = pipeline_regs[3].preg_rd_index;

logic [31:0] div_res_signed_high32;
logic [31:0] div_res_signed_low32;
logic [31:0] div_res_unsigned_high32;
logic [31:0] div_res_unsigned_low32;
logic [31:0] mod_res_signed_high32;
logic [31:0] mod_res_signed_low32;
logic [31:0] mod_res_unsigned_high32;
logic [31:0] mod_res_unsigned_low32;

sign_divider u_sign_divider (
    .aclk(clk),
    .s_axis_dividend_tdata(rj_val),
    .s_axis_divisor_tdata(rk_val),
    .m_axis_dout_tdata({div_res_signed_high32, div_res_signed_low32})
);

unsign_divider u_unsign_divider (
    .aclk(clk),
    .s_axis_dividend_tdata(rj_val),
    .s_axis_divisor_tdata(rk_val),
    .m_axis_dout_tdata({div_res_unsigned_high32, div_res_unsigned_low32})
);

// 输出PRF读取索引
always_comb begin
    prf_read_rj_index = issued_info.issued_preg_rj;
    prf_read_rk_index = issued_info.issued_preg_rk;
end

// FU就绪信号
always_comb begin
    fu_ready = 1'b1; // complex FU总是就绪，因为使用移位寄存器处理延迟
end

// comb logic to decide result
always_comb begin
    if (pipeline_regs[3].gen_op_type == `GENERAL_OPTYPE_3R) begin
        case (pipeline_regs[3].spec_op_type)
            `_3R_DIV: begin
                result = div_res_signed_high32;
            end
            `_3R_MOD: begin
                result = mod_res_signed_low32;
            end
            `_3R_DIVU: begin
                result = div_res_unsigned_high32;
            end
            `_3R_MODU: begin
                result = mod_res_unsigned_low32;
            end
            default: begin
                result = 32'b0;
            end
        endcase
    end else begin
        result = 32'b0;
    end
end

endmodule