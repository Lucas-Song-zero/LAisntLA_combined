# 注意位宽和数组数量的区别
logic [1:0] bar_type_vec [3:0] 是4个2bit数组
logic [3:0] bar_type_vec [1:0] 是2个4bit数组

# Decoder写的还是不够
现在最理想的情况是把decoder做尽可能多的并行的信号流出，尽可能的把所有FU需要使用的信号都提前给提取 or 计算出来
这样的话，至少可以降低FU需要的执行时间，从而让Issue优化之后效果可能可以更好（画饼）
最后又想了想，好像还是不行的，因为最后还是需要根据不同的指令类型进行不同的运算，decoder直接包办FU的判断工作还是有点难了

# 可以给LSU增加一个负责处理不知道什么时候会回来valid的Load/Store指令的通路
这个通路和hit的load是分开的，或者说应该是hit的load单独走一个通知ROB和广播的通路？

# 需要再给CSR unit留一个buffer用于存放其imm值

# hit类的CACOP到底能否提前执行是一个问题，RDCNT是否需要保证顺序执行（感觉也要
但是目前先这样写着？x，RDCNT提前执行如果不报错的话会导致性能虚高？）

# IFU要增加指令缓存功能

# 现在在写rename，RAT和freelist关于rd0的规范需要注意一下
arch_rd_0永远都只能映射到PRF preg_0

# FU写回忙表的端口好像还没给？可能需要增加一下

# 