#include "../common.h"

.section .text.init
.global _start
_start:
    init_all_regs
    la sp, __BINARY_END_
    mv a0,tp
    addi sp,sp,1024
LOOP:
    beq a0,zero,PROGRAM
    addi sp,sp,1024
    addi a0,a0,-1
PROGRAM:

    call main

    mv x3,a0

    j .