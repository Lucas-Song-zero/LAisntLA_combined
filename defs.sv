// 解码输出结构体定义
typedef struct packed {
    logic [3:0] gen_op_type;     // 通用操作类型
    logic [4:0] spec_op_type;    // 特定操作类型
    logic [25:0] imm;            // 立即数值
    logic imm_enable;            // 立即数使能
    logic [4:0] reg_rd;          // 目标寄存器
    logic [4:0] reg_rj;          // 源寄存器1
    logic [4:0] reg_rk;          // 源寄存器2
    logic reg_rd_exist;          // 目标寄存器存在
    logic reg_rj_exist;          // 源寄存器1存在
    logic reg_rk_exist;          // 源寄存器2存在
    logic valid;                 // 有效信号
} decoded_instr_t; 