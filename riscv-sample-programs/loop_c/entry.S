#include "../common.h"

.section .text.init
.global _start
_start:
    init_threads

    la a0, loop_count;
    lw a0, 0(a0);
    call main

    mv x3,a0

    j .

.section .data
.align 4
.global loop_count
loop_count:
    .word 5