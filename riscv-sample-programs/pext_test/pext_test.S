#include "../common.h"

_start:
init_all_regs
li a0, 0x0123456789ABCDEF
// add8 a1,a0,a0
.insn r 0b1110111, 0b000, 0b0100100, a1, a0, a0
.insn r 0b1110111, 0b000, 0b0100000, a2, a0, a0
.insn r 0b1110111, 0b010, 0b0100000, a3, a0, a0

j .