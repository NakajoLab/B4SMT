_start:
    addi x1, x0, 1
    slli x1, x1, 31

    addi x2, x0, 10

    addi x3, x0, -16

    sd x2,0(x1)
    ld x4,0(x1)

    add x3, x2, x3
    add x2, x2, x4

    addi x3, x0, 1
    addi x3, x3, 1
    addi x3, x3, 1
    addi x3, x3, 1
    addi x3, x3, 1
    addi x3, x3, 1
    addi x3, x3, 1
    addi x3, x3, 1
    addi x3, x3, 1
    addi x3, x3, 1
    addi x3, x3, 1

    addi x4, x0, 1025

    sd x3, 0(x1)
    sb x4, 8(x1)
    ld x4, 0(x1)
    lb x3, 8(x1)

    j .