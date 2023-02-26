#include "../common.h"

_start:
    init_all_regs
    addi x1, x0, 1
    slli x1, x1, 31

    li x2, 100
    li x3, 200

    #sd x2, 0(x1)
    #sd x3, 8(x1)
    #ld x2, 0(x1)
    #ld x3, 8(x1)
    #sd x2, 0(x1)
    #sd x3, 8(x1)
    #ld x2, 0(x1)
    #ld x3, 8(x1)

    sd x2, 0(x1)
    sd x3, 8(x1)
    ld x2, 8(x1)
    ld x2, 0(x1)
    ld x3, 0(x1)
    ld x3, 8(x1)


    #sd x2, 0(x1)
    #sd x3, 8(x1)
    #ld x2, 8(x1)
    #ld x3, 0(x1)

    #sd x2, 0(x1)
    #ld x2, 0(x1)
    #sd x3, 8(x1)
    #ld x3, 8(x1)
    #sd x2, 0(x1)
    #ld x2, 0(x1)
    #sd x3, 8(x1)
    #ld x3, 8(x1)

    addi x2,x2,1
    addi x3,x3,1

    j .