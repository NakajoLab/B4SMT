#include "../common.h"

.section .text.init
.global _start
_start:
    li x1,'A'
    li x2,10
    la x3, __IO_ADDR_
0:
    sd x1,0(x3)
    beqz x2, 1f
    addi x1,x1,1
    addi x2,x2,-1
    j 0b
1:
    la x4, TRAP
    csrw mtvec, x4
    addi x3, x3, -64
    sb x1,0(x3)
    li x1, 'X'
    j END

.align 8
TRAP:
    li x1, 'O'
    csrr x2, mcause
    la x3, __IO_ADDR_
    nop
    nop
    nop
    nop
    sb x1, 0(x3)
    j END

END:
    j END
