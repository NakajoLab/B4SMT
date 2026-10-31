// See LICENSE for license details.

//**************************************************************************
// Median filter (c version)
//--------------------------------------------------------------------------

#include "median.h"
#include "encoding.h"

#define ucmplt8(out, in1, in2)                                                 \
  asm volatile(".insn r 0b1110111, 0, 0b0010111, %[out_], %[in1_],%[in2_]"     \
               : [out_] "=r"(out)                                              \
               : [in1_] "r"(in1), [in2_] "r"(in2))

void median(int n, DTYPE input[], DTYPE results[]) {
  for (int i = 0; i < 50; i++) {
    unsigned long v1 = ((long *)input)[i];
    unsigned long vt = ((long *)input)[i + 1];

    unsigned long v2 = (v1 >> 8) | (vt << 56);
    unsigned long v3 = (v1 >> 16) | (vt << 48);

    // printf("1 = %x\n2 = %x\n3 = %x\n", v1, v2, v3);

    unsigned long c1, c2, c3;
    ucmplt8(c1, v1, v2); // A < B
    ucmplt8(c2, v2, v3); // B < C
    ucmplt8(c3, v3, v1); // C < A

    unsigned long m1 = ~(c1 ^ c3);
    unsigned long m2 = ~(c1 ^ c2);
    unsigned long m3 = ~(c2 ^ c3);

    unsigned long o1 = v1 & m1;
    unsigned long o2 = v2 & m2;
    unsigned long o3 = v3 & m3;

    unsigned long res = o1 | o2 | o3;
    unsigned long res_top = res >> 56;

    results[i * 8 + 1] = (unsigned char)(res >> 0);
    *((short *)&results[i * 8 + 2]) = (unsigned short)(res >> 8);
    *((int *)&results[i * 8 + 4]) = (unsigned int)(res >> 24);
    results[i * 8 + 8] = (unsigned char)res_top;

    // results[i * 8 + 1] = (char)(res >> 0);
    // results[i * 8 + 2] = (char)(res >> 8);
    // results[i * 8 + 3] = (char)(res >> 16);
    // results[i * 8 + 4] = (char)(res >> 24);
    // results[i * 8 + 5] = (char)(res >> 32);
    // results[i * 8 + 6] = (char)(res >> 40);
    // results[i * 8 + 7] = (char)(res >> 48);
    // results[i * 8 + 8] = (char)(res >> 56);
  }

  results[0] = 0;
  results[n - 1] = 0;
  return;
}