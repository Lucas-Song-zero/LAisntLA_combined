// typical out-of-order dispatch queue
// becuase add,sub , logic ops , shift ops are all completed in 1 clk
// so we can simply use a pre-wakeup + forward logic to avoid the instrs gap
// besides, we believe mul can also be completed in 1 clk
`include "mycpu.h"

module simple_issue_queue(
    input logic clk,
    input logic rst_n,
    // interface with rename and FU
    input logic rename_valid,
    input logic fu_ready,
    input logic [2:0] rename_instr_num, // 001: 1 instr, 010: 2 instr, 011: 3 instr, 100: 4 instr
    // 指令信息数组
    input simple_single_instr_info_t instr_info [3:0],
    output logic [2:0] left_IQ_entry_cnt, // 剩余的IQ entry数量（只关心0~4条，多于4个空位的情况一律按照5条处理）
    output logic issue_ready, // simple IQ 是否有空间用于发射
    output logic issue_valid, // 发射给FU的数据是否有效

    // wake_up signal
    // wake-up signals inside the queue, if one instr is selected in ALU_IQ
    // then in this cycle, we will wake up the related instrs
    // but the inside wake up signals don't need io
    // outer wake-up input is preg_rd
    input logic [3:0] write_back_rd_exist_vec,
    input logic [`PREG_INDEX_WIDTH-1:0] write_back_rd_index_vec [3:0],

    // 两个simple IQ之间的pre wake-up信息
    input logic [1:0] pre_wakeup_valid,
    input logic [`PREG_INDEX_WIDTH-1:0] pre_wakeup_rd_index_vec [1:0],

    // when issue is ready, give out some info to FU
    output simple_issue_queue_issued_info_t issued_info
);

// a compressed queue for maintaining the instr order
SIMPLE_IQ_ENTRY_t simple_IQ_queue [`IQ_DEPTH-1:0]; // typical 32 instr entry
// at one cycle, at most issue one instr

// besides preg_IQ, we have imm_IQ and op_type_IQ
// because we don't need to change imm_IQ and op_type_IQ when wake-up
// so we decide to split them into 3 queus to accelerate read-out and change speed
logic [25:0] imm_issue_queue [`IQ_DEPTH-1:0];
logic [8:0] op_type_issue_queue [`IQ_DEPTH-1:0];
logic [31:0] pc_issue_queue [`IQ_DEPTH-1:0]; // instr's pc
logic [31:0] pred_jump_pc_issue_queue [`IQ_DEPTH-1:0]; // instr's predicted jump pc
logic [31:0] fetch_start_pc_issue_queue [`IQ_DEPTH-1:0]; // instr's 所在取指块的 fetch start pc
// only BPU predicted taken branch instr will have this pred jump pc, normal instr will simply have pc+4

// and if one instr is issued, we compress the following instrs in the queue
// because it's an compressed queue, we don't need queue_head
logic [`IQ_INDEX_WIDTH:0] filled_entry_cnt; // the number of filled entries in the queue (0~32)

alwasy_comb begin
    if(filled_entry_cnt < `IQ_DEPTH-4) begin // 剩余空位超过4条，一律按照5条处理
        left_IQ_entry_cnt = 3'd5;
    end else begin
        left_IQ_entry_cnt = filled_entry_cnt[2:0]; // 0~4条 (逻辑验证这个是对的)
    end
end

logic alloc_success;
assign alloc_success = (filled_entry_cnt+rename_instr_num <= `IQ_DEPTH);
assign issue_ready = alloc_success;

// 原本的shadow变量法是不能用的(纯是胡编乱造！)
// 并且因为是压缩队列，所以这里反而可以实现比较简单的内部发射（and唤醒？）逻辑
// 8个entry为一组，分成4组
logic [3:0] high_level_ready_vec; // 表明四个大组(&*4=32)是否有ready的指令
logic [7:0] low_level_ready_vec [3:0]; // 表明每个大组内部8个entry是否准备好
logic [30:0] compress_recv_from_behind_en_vec; // 表明哪些entry需要向前写
// 对应位的entry需要从后面一级接受数据 (从第一个entry到倒数第二个entry)
logic [4:0] issue_pos; // 表明本周期需要发射的指令的位置

// entry 0~7 , 8~15 , 16~23 , 24~31
// 使用genvar语句生成?
// 使用genvar生成每组8个entry的ready判断逻辑
genvar group;
genvar entry;
generate
    for(group=0; group<4; group++) begin: ready_group
        // 每组8个entry的ready判断
        for(entry=0; entry<8; entry++) begin: ready_entry
            assign low_level_ready_vec[group][entry] = 
                (simple_IQ_queue[group*8 + entry].issued == 1'b0) &&
                (simple_IQ_queue[group*8 + entry].rj_ready || !simple_IQ_queue[group*8 + entry].rj_valid) &&
                (simple_IQ_queue[group*8 + entry].rk_ready || !simple_IQ_queue[group*8 + entry].rk_valid);
        end
        
        // 每组是否有ready的指令
        assign high_level_ready_vec[group] = |low_level_ready_vec[group];
    end
endgenerate

// 生成压缩使能信号以及发射位置信号
always_comb begin
    casez (high_level_ready_vec)
        4'b0000: begin
            // 没有发射的指令
            compress_recv_from_behind_en_vec = 31'b0;
            issue_pos = 5'b0;
        end
        4'b???1: begin
            // 第一大组有发射的
            compress_recv_from_behind_en_vec[30:8] = 23'b1;
            casez (low_level_ready_vec[0])
                8'b???????1: begin
                    compress_recv_from_behind_en_vec[7:0] = 8'b1111_1111;
                    issue_pos = 5'd0;
                end
                8'b??????10: begin
                    compress_recv_from_behind_en_vec[15:8] = 8'b1111_1110;
                    issue_pos = 5'd1;
                end
                8'b?????100: begin
                    compress_recv_from_behind_en_vec[23:16] = 8'b1111_1100;
                    issue_pos = 5'd2;
                end
                8'b????1000: begin
                    compress_recv_from_behind_en_vec[31:24] = 8'b1111_1000;
                    issue_pos = 5'd3;
                end
                8'b???10000: begin
                    compress_recv_from_behind_en_vec[31:24] = 8'b1111_0000;
                    issue_pos = 5'd4;
                end
                8'b??100000: begin
                    compress_recv_from_behind_en_vec[31:24] = 8'b1110_0000;
                    issue_pos = 5'd5;
                end
                8'b?1000000: begin
                    compress_recv_from_behind_en_vec[31:24] = 8'b1100_0000;
                    issue_pos = 5'd6;
                end
                8'b10000000: begin
                    compress_recv_from_behind_en_vec[31:24] = 8'b1000_0000;
                    issue_pos = 5'd7;
                end
            endcase
        end
        4'b??10: begin
            // 第二大组有发射的
            compress_recv_from_behind_en_vec[30:16] = 15'b1;
            compress_recv_from_behind_en_vec[7:0] = 8'b0;
            casez (low_level_ready_vec[1])
                8'b???????1: begin
                    compress_recv_from_behind_en_vec[15:8] = 8'b1111_1111;
                    issue_pos = 5'd8;
                end
                8'b??????10: begin
                    compress_recv_from_behind_en_vec[15:8] = 8'b1111_1110;
                    issue_pos = 5'd9;
                end
                8'b?????100: begin
                    compress_recv_from_behind_en_vec[15:8] = 8'b1111_1100;
                    issue_pos = 5'd10;
                end
                8'b????1000: begin
                    compress_recv_from_behind_en_vec[15:8] = 8'b1111_1000;
                    issue_pos = 5'd11;
                end
                8'b???10000: begin
                    compress_recv_from_behind_en_vec[15:8] = 8'b1111_0000;
                    issue_pos = 5'd12;
                end
                8'b??100000: begin
                    compress_recv_from_behind_en_vec[15:8] = 8'b1110_0000;
                    issue_pos = 5'd13;
                end
                8'b?1000000: begin
                    compress_recv_from_behind_en_vec[15:8] = 8'b1100_0000;
                    issue_pos = 5'd14;
                end
                8'b10000000: begin
                    compress_recv_from_behind_en_vec[15:8] = 8'b1000_0000;
                    issue_pos = 5'd15;
                end
            endcase
        end
        4'b?100: begin
            // 第三大组有发射的
            compress_recv_from_behind_en_vec[30:24] = 7'b1;
            compress_recv_from_behind_en_vec[15:0] = 16'b0;
            casez (low_level_ready_vec[2])
                8'b???????1: begin
                    compress_recv_from_behind_en_vec[23:16] = 8'b1111_1111;
                    issue_pos = 5'd16;
                end
                8'b??????10: begin
                    compress_recv_from_behind_en_vec[23:16] = 8'b1111_1110;
                    issue_pos = 5'd17;
                end
                8'b?????100: begin
                    compress_recv_from_behind_en_vec[23:16] = 8'b1111_1100;
                    issue_pos = 5'd18;
                end
                8'b????1000: begin
                    compress_recv_from_behind_en_vec[23:16] = 8'b1111_1000;
                    issue_pos = 5'd19;
                end
                8'b???10000: begin
                    compress_recv_from_behind_en_vec[23:16] = 8'b1111_0000;
                    issue_pos = 5'd20;
                end
                8'b??100000: begin
                    compress_recv_from_behind_en_vec[23:16] = 8'b1110_0000;
                    issue_pos = 5'd21;
                end
                8'b?1000000: begin
                    compress_recv_from_behind_en_vec[23:16] = 8'b1100_0000;
                    issue_pos = 5'd22;
                end
                8'b10000000: begin
                    compress_recv_from_behind_en_vec[23:16] = 8'b1000_0000;
                    issue_pos = 5'd23;
                end
            endcase
        end
        4'b1000: begin
            // 第四大组有发射的
            // 前面都没有ready，直接看最后一组
            compress_recv_from_behind_en_vec[23:0] = 24'b0;
            casez (low_level_ready_vec[3])
                8'b???????1: begin
                    compress_recv_from_behind_en_vec[30:24] = 7'b111_1111;
                    issue_pos = 5'd24;
                end
                8'b??????10: begin
                    compress_recv_from_behind_en_vec[31:24] = 7'b111_1110;
                    issue_pos = 5'd25;
                end
                8'b?????100: begin
                    compress_recv_from_behind_en_vec[31:24] = 8'b111_1100;
                    issue_pos = 5'd26;
                end
                8'b????1000: begin
                    compress_recv_from_behind_en_vec[31:24] = 8'b111_1000;
                    issue_pos = 5'd27;
                end
                8'b???10000: begin
                    compress_recv_from_behind_en_vec[31:24] = 8'b111_0000;
                    issue_pos = 5'd28;
                end
                8'b??100000: begin
                    compress_recv_from_behind_en_vec[31:24] = 8'b110_0000;
                    issue_pos = 5'd29;
                end
                8'b?1000000: begin
                    compress_recv_from_behind_en_vec[31:24] = 8'b100_0000;
                    issue_pos = 5'd30;
                end
                8'b10000000: begin
                    compress_recv_from_behind_en_vec[31:24] = 8'b000_0000;
                    issue_pos = 5'd31;
                end
            endcase
        end
    endcase
end

// 这里需要做wake up的并行比较
logic [31:0] wakeup_rj_vec;
logic [31:0] wakeup_rk_vec;
// 下面先进行rj,rk和输入写回寄存器号的比较
always_comb begin
    for(int i=0; i<32; i=i+1) begin
        wakeup_rj_vec[i] = 1'b0;
        wakeup_rk_vec[i] = 1'b0;
        if(simple_IQ_queue[i].rj_valid) begin
            // 比较write_back端口
            for(int j=0; j<4; j=j+1) begin
                if(write_back_rd_exist_vec[j] && (simple_IQ_queue[i].rj_index == write_back_rd_index_vec[j])) begin
                    wakeup_rj_vec[i] = 1'b1;
                end
            end
            // 比较pre_wakeup端口
            for(int k=0; k<2; k=k+1) begin
                if(pre_wakeup_rd_index_vec[k] && (simple_IQ_queue[i].rj_index == pre_wakeup_rd_index_vec[k])) begin
                    wakeup_rj_vec[i] = 1'b1;
                end
            end
        end

        // 同样对rk进行比较
        if(simple_IQ_queue[i].rk_valid) begin
            for(int j=0; j<4; j=j+1) begin
                if(write_back_rd_exist_vec[j] && (simple_IQ_queue[i].rk_index == write_back_rd_index_vec[j])) begin
                    wakeup_rk_vec[i] = 1'b1;
                end
            end
            for(int k=0; k<2; k=k+1) begin
                if(pre_wakeup_rd_index_vec[k] && (simple_IQ_queue[i].rk_index == pre_wakeup_rd_index_vec[k])) begin
                    wakeup_rk_vec[i] = 1'b1;
                end
            end
        end
    end
end
// 这样每个entry是否要唤醒已经计算出来了

// 下面进行发射以及内部的压缩
always_ff @(posedge clk or negedge rst_n) begin
    if(!rst_n || flush) begin
        // 清空队列
        for(int i=0; i<32; i=i+1) begin
            simple_IQ_queue[i].issued <= 1'b0;
            simple_IQ_queue[i].rj_ready <= 1'b0;
            simple_IQ_queue[i].rk_ready <= 1'b0;
            simple_IQ_queue[i].rj_valid <= 1'b0;
            simple_IQ_queue[i].rk_valid <= 1'b0;
        end
        filled_entry_cnt <= 0;
    end
    // 这里我们的提前唤醒就是很正常的了
    
end

endmodule