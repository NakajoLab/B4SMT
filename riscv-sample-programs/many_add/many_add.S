#include "../common.h"

_start:
init_all_regs
li x1,1
li x2,1
li x3,1
li x4,1
li x5,1
li x6,1
li x7,1
li x8,1
add x1,x1,x2
add x3,x3,x4
add x5,x5,x6
add x7,x7,x8
add x1,x1,x3
add x5,x5,x7
add x1,x1,x5

j .