#include "../common.h"

_start:
    init_all_regs
    la x1, __var_place

    addi x2, x0, 10

    sd x2,0(x1)
    ld x3,0(x1)

    j .

.section .data
__var_place: