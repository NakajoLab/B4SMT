#include "../common.h"

.section .text.init
.global _start
_start:
    init_all_regs
    csrrs tp,mhartid,zero
1:
    bnez tp,1b

    la tp, trap
    csrw mtvec,tp
    li a0, 10
    unimp
    li a0, 20
1:
    j 1b

.align 4
trap:
    csrr tp,mcause
    li t0, 2
    beq tp,t0,success
    li a0,30
1:
    j 1b

success:
    li a0,40
1:
    j 1b
