#include "../common.h"

.section .text.init
.global _start
_start:
    init_all_regs
    li sp, 1
    slli sp, sp, 31
    addi sp, sp, 512

    call main

    mv x3,a0

    j .