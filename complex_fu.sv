`include "defs.sv"

module complex_fu (
    input logic clk,
    input logic rst_n,
    input logic [3:0] gen_op_type,
    input logic [4:0] spec_op_type,

    // interface with complex_IQ
    // verified that the IP core--div can pipeline output
    input logic [`ROB_ENTRY_INDEX_WIDTH-1:0] rob_entry_index,
    input logic IQ_valid,
    output logic FU_ready,

    // read from PRF or imm or bypass
    input logic [31:0] rj_val,
    input logic [31:0] rk_val,
    input logic [`PREG_INDEX_WIDTH-1:0] preg_rd_index,
    // no need for pc and pred_taken (complex_FU simply handle div and mod)

    // write to PRF
    output logic [31:0] result,
    output logic [`PREG_INDEX_WIDTH-1:0] wb_rd_index,
    output logic [`ROB_ENTRY_INDEX_WIDTH-1:0] wb_rob_entry_index,
    output logic result_valid
);

// after one cycle delay, pass the preg rd index to the output
always@ (posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        wb_rd_index <= 0;
    end else begin
        wb_rd_index <= preg_rd_index;
    end
end

// after one cycle delay, pass the rob_entry_index to the output
always@ (posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        wb_rob_entry_index <= 0;
    end else begin
        wb_rob_entry_index <= rob_entry_index;
    end
end

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

// always@ shift_reg method to handle the multi-cycle delay
// the left bit is the data_valid bit (IQ_valid)
// after 4 cycles delay ,the output valid bit will be set 1
logic [`COUNTING_DOWN_BITS_WIDTH:0] delay_shift_reg; // actually is [4:0]

// at each cycle the left bit will be set equal to data_valid
// and each cycle the regs will be right shifted 1 bit
// the most right bit is the output valid bit
always@ (posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        delay_shift_reg <= {(`COUNTING_DOWN_BITS_WIDTH+2){1'b0}};
    end else begin
        delay_shift_reg <= {IQ_valid, delay_shift_reg[`COUNTING_DOWN_BITS_WIDTH+1:1]};
    end
end

always_comb begin
   res_valid = delay_shift_reg[0]; // the most right bit is the output valid bit
end

// comb logic to decide result
always_comb begin
    if (gen_op_type == `GENERAL_OPTYPE_3R) begin
        case (spec_op_type)
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