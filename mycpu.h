/*
BPU 
目前设计的比较简陋
只有BHT和uBTB，后面希望可以加上TAGE
*/
// Branch Prediction Unit (BPU) definitions

// Branch History Table (BHT) configuration
`define BHT_SIZE 1024
`define BHT_WIDTH 2
`define BHT_INDEX_WIDTH $clog2(`BHT_SIZE)  // 10 bits for 1024 entries

// Unified Branch Target Buffer (uBTB) configuration
`define uBTB_SIZE `BHT_SIZE  // Keep consistent with BHT_SIZE
`define uBTB_INDEX_WIDTH $clog2(`uBTB_SIZE)  // 10 bits for 1024 entries

// Branch prediction states
`define STRONGLY_NOT_TAKEN 2'b00
`define WEAKLY_NOT_TAKEN   2'b01
`define WEAKLY_TAKEN       2'b10
`define STRONGLY_TAKEN     2'b11

// Branch types
`define BRANCH_TYPE_DIRECT   2'b00
`define BRANCH_TYPE_INDIRECT 2'b01
`define BRANCH_TYPE_RETURN   2'b10
`define BRANCH_TYPE_CALL     2'b11

// Valid bit
`define VALID 1'b1
`define INVALID 1'b0 

/////////////////////////////////////////////
////////////////////////////////////////////

/*
IFU(IFU包含了Instr Fetch Buffer和执行取指的IFU)
才发现没写IFU，主要当时因为Icache还没写，所以IFU也没写
*/
// IFB: instr fetch buffer
`define IFB_DEPTH 32
`define IFB_INDEX_WIDTH 5

// IFB entry definition
typedef struct packed {
    logic [31:0] fetch_pc;
    logic [1:0] cut_pos;
} IFB_ENTRY_t;

/////////////////////////////////////////////
////////////////////////////////////////////

/*
Decode Stage
decoder用于将IFU取出的指令进行解码，并且还需要进行B、BL的提前判断
decoder还需要注意exception的提前给出（各种CSR指令）
*/
`define GENERAL_OPTYPE_RDCNT 4'b0001
`define GENERAL_OPTYPE_BAR 4'b0010
`define GENERAL_OPTYPE_ATOMIC 4'b0011
`define GENERAL_OPTYPE_3R 4'b0100
`define GENERAL_OPTYPE_BJ 4'b0101
`define GENERAL_OPTYPE_CSR 4'b0110
`define GENERAL_OPTYPE_SYSC_BRK 4'b0111
`define GENERAL_OPTYPE_2R5I 4'b1000
`define GENERAL_OPTYPE_1R20I 4'b1001
`define GENERAL_OPTYPE_2R12I 4'b1010

// universal invalid op - define for different bit widths
`define INVALID_OP_2B 2'b00
`define INVALID_OP_4B 4'b0000
`define INVALID_OP_5B 5'b00000

// For backward compatibility
`define INVALID_OP 2'b00

// RDCNT -> 3 instrs (op_type 2bits: 00=invalid, 01=RDCNTID, 10=RDCNTVL, 11=RDCNTVH)
`define RDCNT_RDCNTID 2'b01
`define RDCNT_RDCNTVL 2'b10
`define RDCNT_RDCNTVH 2'b11

// 3R -> 18 instrs (op_type 5bits: 00000=invalid, 00001-10010=18 instructions)
`define _3R_ADD 5'b00001
`define _3R_SUB 5'b00010
`define _3R_SLT 5'b00011
`define _3R_SLTU 5'b00100
`define _3R_NOR 5'b00101
`define _3R_AND 5'b00110
`define _3R_OR 5'b00111
`define _3R_XOR 5'b01000
`define _3R_SLL 5'b01001
`define _3R_SRL 5'b01010
`define _3R_SRA 5'b01011
`define _3R_MUL 5'b01100
`define _3R_MULH 5'b01101
`define _3R_MULHU 5'b01110 // two operands saw as unsigned, put higher bits into rd
`define _3R_DIV 5'b01111
`define _3R_MOD 5'b10000
`define _3R_DIVU 5'b10001 // two operands saw as unsigned
`define _3R_MODU 5'b10010 // two operands saw as unsigned 

// SYSC_BRK -> 2 instrs (op_type 2bits: 00=invalid, 01=SYSCALL, 10=BREAK)
`define SB_SYSCALL 2'b01
`define SB_BREAK 2'b10

// 2r5i -> 3 instrs (op_type 2bits: 00=invalid, 01=SLLI, 10=SRLI, 11=SRAI)
`define _2R5I_SLLI 2'b01
`define _2R5I_SRLI 2'b10
`define _2R5I_SRAI 2'b11

// 2r12i -> 6 + 9 = 15 instrs (op_type 4bits: 0000=invalid, 0001-1111=15 instructions)
`define _2R12I_SLTI 4'b0001
`define _2R12I_SLTUI 4'b0010
`define _2R12I_ADDI 4'b0011
`define _2R12I_ANDI 4'b0100
`define _2R12I_ORI 4'b0101
`define _2R12I_XORI 4'b0110

`define _2R12I_LD_B 4'b0111 // load byte
`define _2R12I_LD_H 4'b1000 // load half word
`define _2R12I_LD_W 4'b1001
`define _2R12I_ST_B 4'b1010 // store byte
`define _2R12I_ST_H 4'b1011
`define _2R12I_ST_W 4'b1100
`define _2R12I_LD_BU 4'b1101
`define _2R12I_LD_HU 4'b1110
`define _2R12I_PRELD 4'b1111

// csr -> 11 instrs (op_type 4bits: 0000=invalid, 0001-1011=11 instructions)
`define CSR_CSRRD 4'b0001
`define CSR_CSRWR 4'b0010
`define CSR_CSRXCHG 4'b0011
`define CSR_CACOP 4'b0100
`define CSR_TLBSRCH 4'b0101
`define CSR_TLBRD 4'b0110
`define CSR_TLBWR 4'b0111
`define CSR_TLBFILL 4'b1000
`define CSR_ERTN 4'b1001
`define CSR_IDLE 4'b1010
`define CSR_INVTLB 4'b1011

// 1r20i -> 2 instrs (op_type 2bits: 00=invalid, 01=LU12I, 10=PCADDU12I)
`define _1R20I_LU12I 2'b01
`define _1R20I_PCADDU12I 2'b10

// atomic -> 2 instrs (op_type 2bits: 00=invalid, 01=LL, 10=SC)
`define ATOMIC_LL 2'b01
`define ATOMIC_SC 2'b10

// bar -> 2 instrs (op_type 2bits: 00=invalid, 01=DBAR, 10=IBAR)
`define BAR_DBAR 2'b01
`define BAR_IBAR 2'b10

// BJ -> 9 instrs (op_type 4bits: 0000=invalid, 0001-1001=9 instructions)
`define BJ_JIRL 4'b0001
`define BJ_B 4'b0010
`define BJ_BL 4'b0011
`define BJ_BEQ 4'b0100
`define BJ_BNE 4'b0101
`define BJ_BLT 4'b0110
`define BJ_BGE 4'b0111
`define BJ_BLTU 4'b1000
`define BJ_BGEU 4'b1001

// 解码输出结构体定义
typedef struct packed {
    logic [3:0] gen_op_type;     // 通用操作类型
    logic [4:0] spec_op_type;    // 特定操作类型
    logic [25:0] imm;            // 立即数值
    logic imm_enable;            // 立即数使能
    logic imm_sign_extend;       // 立即数符号扩展
    logic [4:0] reg_rd;          // 目标寄存器
    logic [4:0] reg_rj;          // 源寄存器1
    logic [4:0] reg_rk;          // 源寄存器2
    logic reg_rd_exist;          // 目标寄存器存在
    logic reg_rj_exist;          // 源寄存器1存在
    logic reg_rk_exist;          // 源寄存器2存在
    logic valid;                 // 有效信号
    logic [`ROB_MAINBODY_ENTRY_exception_width-1:0] exception; // 异常信号
    logic store_or_load;         // 是否是store或load指令
    logic [1:0] bar_type;        // 屏障类型 00 - normal , 01 - DBAR , 10 - IBAR
    logic completion_bit;        // 一些指令需要保证发射即完成，比如PRELD等
    logic [1:0] issue_distr_direction; // 00 - simple_IQ ; 01 - complex_IQ ; 10 - LSU_IQ ; 11 - ROB
} decoded_instr_t; 

/////////////////////////////////////////////
////////////////////////////////////////////

/*
rename
rename 进行对架构寄存器的重命名，renamed成物理寄存器号
*/
`define PRF_NUM 128
`define PREG_INDEX_WIDTH 7  // log2(128) = 7
`define RAT_ENTRY_WIDTH 7   // log2(128) = 7

// rename输入结构体定义
typedef struct packed {
    logic [3:0] gen_op_type;            // 指令通用操作类型
    logic [4:0] spec_op_type;           // 指令特定操作类型
    logic [`ROB_MAINBODY_ENTRY_exception_width-1:0] exception; // 异常信号类型
    logic [25:0] imm;                   // 指令立即数
    logic imm_enable;                   // 指令立即数使能
    logic imm_sign_extend;              // 指令立即数符号扩展
    logic valid;                        // 指令有效
    logic [4:0] reg_rd;                 // rd架构寄存器索引
    logic reg_rd_exist;                 // rd存在
    
    logic [4:0] reg_rj;                 // rj架构寄存器索引
    logic reg_rj_exist;                 // rj存在
    logic [4:0] reg_rk;                 // rk架构寄存器索引
    logic reg_rk_exist;                 // rk存在
    logic store_or_load;                // 是否是store或load指令
    logic [1:0] bar_type;               // 屏障类型 00 - normal , 01 - DBAR , 10 - IBAR
    logic completion_bit;               // 一些指令需要保证发射即完成，比如PRELD等
    logic [1:0] issue_distr_direction;  // 00 - simple_IQ ; 01 - complex_IQ ; 10 - LSU_IQ ; 11 - ROB
    logic [31:0] pred_jump_target_pc;   // 预测的跳转目标PC
} rename_input_t;

// rename输出结构体定义
typedef struct packed {
    logic [31:0] pc;                    // 指令PC
    logic valid;                        // 指令有效
    logic [3:0] gen_op_type;
    logic [4:0] spec_op_type;
    logic [`ROB_MAINBODY_ENTRY_exception_width-1:0] exception; // 异常信号类型
    logic [25:0] imm;                   // 指令立即数
    logic imm_enable;                   // 指令立即数使能
    logic imm_sign_extend;              // 指令立即数符号扩展
    logic [4:0] rd_arch_index;          // rd架构寄存器索引
    logic rd_exist;                     // rd存在
    logic preg_rd_ready;                // rd物理寄存器 value ready
    logic [`PREG_INDEX_WIDTH-1:0] preg_rd;  // rd物理寄存器索引
    logic [`PREG_INDEX_WIDTH-1:0] preg_rj;  // rj物理寄存器索引
    logic rj_exist;                     // rj存在
    logic preg_rj_ready;                // rj物理寄存器 value ready
    logic [`PREG_INDEX_WIDTH-1:0] preg_rk;  // rk物理寄存器索引
    logic rk_exist;                     // rk存在
    logic preg_rk_ready;                // rk物理寄存器 value ready
    logic [`PREG_INDEX_WIDTH-1:0] rd_history_preg;  // rd历史物理寄存器索引
    logic [31:0] pred_jump_target_pc; // 预测的跳转目标pc(针对这条指令来说的)
    logic [1:0] issue_distr_direction; // 00 - simple_IQ ; 01 - complex_IQ ; 10 - LSU_IQ ; 11 - ROB
    logic store_or_load;                // 是否是store或load指令
    logic [1:0] bar_type;               // 屏障类型 00 - normal , 01 - DBAR , 10 - IBAR
    logic completion_bit;               // 一些指令需要保证发射即完成，比如PRELD等
} rename_output_t;

/////////////////////////////////////////////
////////////////////////////////////////////

/*
issue 和 execute
issue 对 rename 后的指令先分配到每个FU的发射队列中，然后每个发射队列内部进行唤醒和发射，每个FU自己接收需要执行的指令
execute 对Issue Queue中发射的指令进行执行，并且将结果写回PRF
execute结果写回Issue Queue，同时再发射到ROB，标记对应entry为完成(可能要改成IQ也把ROB entry Index发给FU，FU完成直接写回）
*/
`define IQ_DEPTH 32
`define IQ_INDEX_WIDTH 5
`define COMPLEX_IQ_DEPTH 32
`define COMPLEX_IQ_INDEX_WIDTH 5

// 使用typedef struct定义之后，连位宽计算都省了
typedef struct packed {
    logic [`ROB_ENTRY_INDEX_WIDTH-1:0] rob_entry_index;
    logic [`PREG_INDEX_WIDTH-1:0] rj_index;
    logic rj_valid;
    logic rj_ready;
    logic [`PREG_INDEX_WIDTH-1:0] rk_index;
    logic rk_valid;
    logic rk_ready;
    logic [`PREG_INDEX_WIDTH-1:0] rd_index;
    logic rd_valid;
    logic pred_taken; // 预测这条指令是否taken
    // logic rd_ready; // 在目前的定义中，只有store是需要使用rd的数据，所以我觉得目前不写rd_ready也是合理的
    logic imm_enable;
    logic issued;
} SIMPLE_IQ_ENTRY_t;

// DIV and MOD latency is 4 clk, so we need to delay 3 clks
// therefore the counting-down bits is 3
`define COUNTING_DOWN_BITS_WIDTH 4
typedef struct packed {
    logic [`ROB_ENTRY_INDEX_WIDTH-1:0] rob_entry_index;
    logic [`PREG_INDEX_WIDTH-1:0] rj_index;
    logic rj_valid;
    logic rj_counting_down_enable;
    logic [`COUNTING_DOWN_BITS_WIDTH-1:0] rj_counting_down_bits; // 4bits for counting down (因为DIV和MOD的latency是4clk，所以需要4bits delay)
    logic [`PREG_INDEX_WIDTH-1:0] rk_index;
    logic rk_valid;
    logic rk_counting_down_enable;
    logic [`COUNTING_DOWN_BITS_WIDTH-1:0] rk_counting_down_bits; // 3bits for counting down (因为DIV和MOD的latency是4clk，所以需要3bits来表示)
    logic [`PREG_INDEX_WIDTH-1:0] rd_index;
    logic rd_valid;
    logic imm_enable;
    logic issued;
} COMPLEX_IQ_ENTRY_t;

// LSU_IQ:
`define LSU_IQ_DEPTH 64
`define LSU_IQ_INDEX_WIDTH 6
// because LSU may be stalled more frequently,
// so we need to use larger buffer to eliminate the penalty

// LSU_IQ entry includes one more bit than simplecalc_IQ:
// the completion bit: 1 means the instr can be poped 
typedef struct packed {
    logic [`ROB_ENTRY_INDEX_WIDTH-1:0] rob_entry_index;
    logic [1:0] bar_type;
    logic store_or_load;
    logic page_diff_index;
    logic [`PREG_INDEX_WIDTH-1:0] store_block_index;
    logic [`PREG_INDEX_WIDTH-1:0] rj_index;
    logic rj_valid;
    logic rj_ready;
    logic [`PREG_INDEX_WIDTH-1:0] rk_index;
    logic rk_valid;
    logic rk_ready;
    logic [`PREG_INDEX_WIDTH-1:0] rd_index;
    logic rd_valid;
    logic rd_ready;
    logic imm_enable;
    logic issued;
    logic completion;
} LSU_IQ_ENTRY_t;

`define STORE_BUFFER_DEPTH 32
`define STORE_BUFFER_INDEX_WIDTH 5
typedef struct packed {
    logic [31:0] rd_data;
    logic [31:0] final_addr;
    logic [3:0] wstrb; // 写字节使能(1111 -- 4bytes=1word, 0011 -- halfword, 0001 -- 1byte)
    logic is_sc;
    logic is_prefetch;
} STORE_BUFFER_ENTRY_t;

// FU result select
`define ADDSUB_RES_SEL 4'b0000
`define MUL_RES_SIGNED_LOW32_SEL 4'b0001
`define MUL_RES_SIGNED_HIGH32_SEL 4'b0010
`define MUL_RES_UNSIGNED_LOW32_SEL 4'b0011
`define MUL_RES_UNSIGNED_HIGH32_SEL 4'b0100
`define SHIFT_RES_SEL 4'b0101
`define COMPARE_RES_SEL 4'b0110
`define LOGIC_RES_SEL 4'b0111
`define BJ_RES_SEL 4'b1000 

// interface with D-Cache
// 缓存请求
typedef struct packed {
    logic [`LSU_IQ_INDEX_WIDTH-1:0] req_index; // fot tracking the instr
    logic [31:0] addr;
    logic [31:0] data;
    logic [3:0] wstrb; // 写字节使能(1111 -- 4bytes=1word, 0011 -- halfword, 0001 -- 1byte)
    logic valid; // 请求有效
    logic is_write; // 是否是写请求
    logic is_ll; // 是否是LL atomic instr
    logic is_sc; // 是否是SC atomic instr
    logic is_prefetch; // 是否是预取指令
    logic is_qry_inv; // Query-Index Cacop 维护请求
    logic qry_writeback; // CACOP维护是否需要写回
    logic [1:0] size; // 访问大小(字节)
    logic unsigned_load; // 无符号加载 (LD.BU / LD.HU)
} cache_req_t;

// 缓存响应
typedef struct packed {
    logic [`ROB_ENTRY_INDEX_WIDTH-1:0] rob_entry_index; // for tracking the instr
    logic [31:0] data; // 读取的数据
    logic [`PREG_INDEX_WIDTH-1:0] preg_rd_index; // rd index
    logic is_write; // 是否是写请求，如果不是，则需要把load的数据写回preg_rd
    logic ready; // 请求完成
    logic sc_success; // SC指令是否成功
    logic hit; // 针对 CACOP query-index 是否命中信号 1=hit
} cache_resp_t;

typedef struct packed {
    logic [4:0] code; // CACOP 操作码
    logic [31:0] addr; // 地址
    logic valid; // 请求有效
} cacop_req_t;

typedef struct packed {
    logic ready; // 请求完成
} cacop_resp_t;

typedef struct packed {
    logic [31:0] addr; // 预取地址
    logic [4:0] hint; // 预取提示
    logic valid; // 请求有效
} preld_req_t;

typedef struct packed {
    logic [`ROB_ENTRY_INDEX_WIDTH-1:0] rob_entry_index; // for tracking the instr
    logic [4:0] hint; // 屏障提示
    logic valid; // 请求有效
    logic is_ibar; // 1:IBAR  0:DBAR
} bar_req_t;

typedef struct packed {
    logic [`ROB_ENTRY_INDEX_WIDTH-1:0] rob_entry_index; // for tracking the instr
    logic ready; // 请求完成
} bar_resp_t;

/////////////////////////////////////////////
////////////////////////////////////////////

/*
Retire
ROB 每个周期尽量多的按序退休指令，目前还没有写CSR的处理
*/
`define ROB_PC_ENTRY_WIDTH 32
`define ROB_DEPTH 128
`define ROB_ENTRY_INDEX_WIDTH 7

`define ROB_MAINBODY_ENTRY_exception_width 6 // 暂时设置为6bits
// exception state
`define EXCEPTION_NONE 6'b000000
`define EXCEPTION_PRED_WRONG 6'b000001
`define EXCEPTION_CSR_INSTR 6'b000010 // 表明这是一条CSR指令，需要CSR unit来处理
`define EXCEPTION_SYSCALL_BREAK 6'b000011 // 表明这是一条SYSCALL或BREAK指令，需要csr unit来处理?
// 后面可能会考虑把SYSCALL_BREAK合并到CSR_INSTR中，因为CSR_INSTR中也会处理SYSCALL和BREAK指令（我希望可以这样处理）
// 其他还有定义的很多例外，现在先不写了

typedef struct packed {
    logic [`ROB_MAINBODY_ENTRY_exception_width-1:0] exception;
    logic completion;
    logic [4:0] arch_rd_index;
    logic [`PREG_INDEX_WIDTH-1:0] preg_rd_index;
    logic [`PREG_INDEX_WIDTH-1:0] rd_history_index;
    logic rd_exist;
    logic [3:0] gen_op_type;
    logic [4:0] spec_op_type;
    logic store_or_load; // 1 -- store ; 0 -- load
} mainbody_entry_t;

typedef struct packed {
    logic [31:0] real_jump_pc;
    logic [2:0] BJ_type; // 0 表示普通分支指令, 1 表示B指令(相当于直接跳转) , 2 表示 BL (相当于JAL) , 3 表示 普通非调用间接跳转指令 , 4 表示 函数返回指令(JR $ra类似于)
} BJ_jump_entry_t;