#include "../common.h"

.section .text.init
.global _start
_start:
    init_all_regs
    li a0,0
    li a1, 0x30000000
    li a2, 0x10000
_LOOP:
    addi a0,a0,1
    sw a0,0(a1)

    call wait

    bne a0,a2,_LOOP
    li a0,0
    j _LOOP

.global wait
wait:
    li a3,1000
_WTMP:
    beq a3,zero,_WEND
    addi a3,a3,-1
    j _WTMP
_WEND:
    ret

END:
    j .