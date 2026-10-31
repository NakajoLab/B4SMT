// See LICENSE for license details.

//**************************************************************************
// Median filter (c version)
//--------------------------------------------------------------------------

#include "median.h"
#include "encoding.h"

#define cmplt16(out, in1, in2)                                                 \
  asm volatile(".insn r 0b1110111, 0, 0b0010110, %[out_], %[in1_],%[in2_]"     \
               : [out_] "=r"(out)                                              \
               : [in1_] "r"(in1), [in2_] "r"(in2))

void median(int n, DTYPE input[], DTYPE results[]) {
  for (int i = 0; i < 100; i++) {
    unsigned long v1 = ((long *)input)[i];
    unsigned long vt = ((long *)input)[i + 1];

    unsigned long v2 = (v1 >> 16) | (vt << 48);
    unsigned long v3 = (v1 >> 32) | (vt << 32);

    // printf("1 = %x\n2 = %x\n3 = %x\n", v1, v2, v3);

    unsigned long c1, c2, c3;
    cmplt16(c1, v1, v2); // A < B
    cmplt16(c2, v2, v3); // B < C
    cmplt16(c3, v3, v1); // C < A

    unsigned long m1 = ~(c1 ^ c3);
    unsigned long m2 = ~(c1 ^ c2);
    unsigned long m3 = ~(c2 ^ c3);

    unsigned long o1 = v1 & m1;
    unsigned long o2 = v2 & m2;
    unsigned long o3 = v3 & m3;

    unsigned long res = o1 | o2 | o3;
    results[i * 4 + 1] = (short)(res >> 0);
    *((int *)&results[i * 4 + 2]) = (int)(res >> 16);
    results[i * 4 + 4] = (short)(res >> 48);
  }

  results[0] = 0;
  results[n - 1] = 0;
  return;
}

const int elements_at_once = 64 / sizeof(DTYPE);
