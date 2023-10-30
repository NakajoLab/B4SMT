long main(long loop_count) {
  int i;
  long vl=1, avl = 29, vtype;
  for(i=0; avl != 0; i++) {
    asm volatile ("vsetvli %0, %1, e32, m1, ta, ma"
    : "=r"(vl)
    : "r"(avl));
    asm volatile ("csrr %0, vtype"
    : "=r"(vtype));
    avl -= vl;
  }
  return 0;
}