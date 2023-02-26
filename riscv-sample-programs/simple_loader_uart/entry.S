#include "../common.h"

.extern __STACK_START_

.section .text.init
.global _start
_start:
    init_all_regs
    li a0,1
    call _show_led

    call _clear_bss
    li a0,2
    call _show_led

    call _set_sp
    li a0,3
    call _show_led

    call _copy_loader_to_ram
    li a0,4
    call _show_led

    call __RAM_START
    j .

.global _show_led
_show_led:
    la a1, __GPIO_START
    sb a1,0(a0)
    ret

.global _copy_loader_to_ram
_copy_loader_to_ram:
    mv a0,tp
    beqz a0, .L_start_loader_copy
    la a0, loader_init_done
    call _wait_for_init
    ret
.L_start_loader_copy:
    la a0, _binary_loader_binary_bin_start
    la a1, _binary_loader_binary_bin_end
    la a2, __RAM_START
.L_loader_copy:
    bge a0,a1,.L_end_loader_copy
    ld a3,0(a0)
    sd a3,0(a2)
    addi a0,a0,8
    addi a2,a2,8
    j .L_loader_copy
.L_end_loader_copy:
    ret


.global _set_sp
_set_sp:
    la sp, __RAM_START
    addi sp,sp,1024
    mv a0,tp
.L_bump_sp:
    beqz a0, .L_end_set_sp
    addi sp,sp,1024
    addi a0,a0,-1
    j .L_bump_sp
.L_end_set_sp:
    ret

.global _clear_bss
_clear_bss:
.L_clear_init_done:
    la a0,bss_init_done
    li a1,0
    sb a1,0(a0)
    beqz tp, .L_init_bss
    la a0,bss_init_done
    call _wait_for_init
    ret
.L_init_bss:
    la a0,__BSS_START
    la a1,__BSS_END
    li a2,0
.L_clear_next:
    sd a2,0(a0)
    bge a0,a1,.L_end_init_bss
    addi a0,a0,8
    j .L_clear_next
.L_end_init_bss:
    la a0,bss_init_done
    li a1,1
    sb a1,0(a0)
    j .L_end_clear_bss
.L_end_clear_bss:
    li a0,0
    li a1,0
    li a2,0
    ret

.global _wait_for_init
_wait_for_init:
    li a2,100
.L_wait_inner:
    beqz a2, .L_check_init
    addi a2,a2,-1
    j .L_wait_inner
.L_check_init:
    lb a1,0(a0)
    bnez a1, .L_end_wait
    j _wait_for_init
.L_end_wait:
    ret

.section .bss
bss_init_done: .byte 0
loader_init_done: .byte 0
