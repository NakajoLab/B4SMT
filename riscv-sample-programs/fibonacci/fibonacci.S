#include "../common.h"

main:
  init_all_regs
  li x2,10              # int n=10;
  li x3,1
  blt x3,x2,L1          # if (1<n) goto L1;
  mv x3,x2
  j L3
L1:
  li x3,1		# int fib=1;
  li x4,1 		# int fibPrev=1;
  li x1,2		# int i=2;
L2:
  bge x1,x2,L3          # if (i>=n) goto L3;
  mv x5,x3		# int temp=fib;
  add x3,x3,x4		# fib+=fibPrev;
  mv x4,x5 		# fibPrev=temp;
  addi x1,x1,1		# i++
  j L2
L3:
  mv x6,x3
  j .