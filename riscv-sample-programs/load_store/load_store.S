#include "../common.h"

_start:
    init_all_regs
    la x1, __var_place

    addi x2, x0, 10
    addi x1,x1,16

    sd x2,-16(x1)
    ld x3,-16(x1)

    j .

.section .data
__var_place: