#include "../common.h"

_start:
    init_all_regs
    la x1, scratchpad

    li x2, 10
    li x4, 20

    sd x2, 0(x1)

    mv x2,x1
    addi x2, x2, 20
    addi x2, x2, 20
    addi x2, x2, 20
    addi x2, x2, 20
    addi x2, x2, -20
    addi x2, x2, -20
    addi x2, x2, -20
    addi x2, x2, -20

    ld x3, 0(x2)
    sd x4, 0(x1)

    j .

.section .data
scratchpad: .dword 0