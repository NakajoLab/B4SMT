#include "../common.h"

.section .text.init
.global _start
_start:
    init_all_regs
    li a0,2
    li a1,3
    add a2,a1,a0
    bne a2,x0,LABEL
    li a3,10
    beq zero,zero,END
LABEL:
    li a3,20

END:
    j .