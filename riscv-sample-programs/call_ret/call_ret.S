#include "../common.h"

.global _start
_start:
    init_all_regs
    li x5,1
    call foo
    li x7,3
    j end

foo:
    li x6,2
    ret

end:
    j .