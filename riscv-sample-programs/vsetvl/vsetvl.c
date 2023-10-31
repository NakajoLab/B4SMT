long main(long loop_count) {
  int i = 0;
  long vl, avl = 29;
  while(avl != 0) {
    // 256bit -> 32 * 8 elements
    // i:0 -> avl = 29
    // i:1 -> avl = 21
    // i:2 -> avl = 13
    // i:3 -> avl = 5
    asm volatile ("vsetvli %0, %1, e32, m1, ta, ma"
    : "=r"(vl)
    : "r"(avl));
    avl -= vl;
    i++;
  }
  return i;
}