#include "../common.h"

.section .text.init
.global _start
_start:
init_threads

    call main

    mv x3,a0

    j .