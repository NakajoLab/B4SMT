#include "../common.h"

_start:
    init_all_regs
    la x1, __BINARY_END_

    li x2, 10

    sd x2,0(x1)

    ld x2, 0(x1)
    ld x3, 0(x1)
    ld x4, 0(x1)

    addi x2, x2, 1
    addi x3, x3, 2
    addi x4, x4, 3

    sd x2, 16(x1)
    sd x3, 8(x1)
    sd x4, 0(x1)

    ld x2, 0(x1)
    ld x3, 8(x1)
    ld x4, 16(x1)

    add x2,x2,x3
    add x2,x2,x4

    j .