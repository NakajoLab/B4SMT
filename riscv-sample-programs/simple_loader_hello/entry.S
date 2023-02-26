#include "../common.h"

.extern __STACK_START_

.section .text.init
.global _start
_start:
    init_all_regs
    csrr tp, mhartid
    bnez tp, END
_write:
    la a0, __GPIO_START
    li a1,0xFF
    sb a1,0(a0)
    la a0, __UART_START
    la a4, hello
NEXT_CHAR:
    lb a5,0(a4)
    beqz a5, loop
check_full:
    lw a7, 0x8(a0)
    andi a7,a7,8
    beqz a7, write_char
    call wait
    j check_full
write_char:
    sb a5,4(a0)
    addi a4,a4,1
    j NEXT_CHAR

loop:
get_char:
    la a0, __UART_START
    lb a1, 0x8(a0)
    andi a1,a1,1
    bnez a1, wait_fifo_empty
    call wait
    j get_char
wait_fifo_empty:
    lb a1, 0x8(a0)
    andi a1,a1,8
    beqz a1, put_char
    call wait
    j wait_fifo_empty
put_char:
    lbu a1, 0x0(a0)
    sb a1, 0x4(a0)
    j loop

END:
    j .


.global wait
wait:
    li a3,200
_WTMP:
    beq a3,zero,_WEND
    addi a3,a3,-1
    j _WTMP
_WEND:
    ret

.section .data
hello: .string "Hello World\r\nFrom B4Processor!\r\n"
