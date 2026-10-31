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

static volatile int finish_count = 0;

const int from_arr[4] = {0, 25, 50, 75};
const int to_arr[4] = {25, 50, 75, 100};

void median(int n, DTYPE input[], DTYPE results[]) {
  int threadid = read_csr(mhartid);

  int from = from_arr[threadid], to = to_arr[threadid];

  for (int i = from; i < to; i++) {
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
    results[i * 4 + 2] = (short)(res >> 16);
    results[i * 4 + 3] = (short)(res >> 32);
    results[i * 4 + 4] = (short)(res >> 48);
  }

  results[0] = 0;
  results[n - 1] = 0;

  int one = 1;
  asm("amoadd.w zero,%[one],(%[ptr])"
      :
      : [one] "r"(one), [ptr] "r"(&finish_count));

  while (finish_count != CORE_COUNT)
    ;

  return;
}

const int elements_at_once = 64 / sizeof(DTYPE);
