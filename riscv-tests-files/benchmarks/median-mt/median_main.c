// See LICENSE for license details.

//**************************************************************************
// Median filter bencmark
//--------------------------------------------------------------------------
//
// This benchmark performs a 1D three element median filter. The
// input data (and reference data) should be generated using the
// median_gendata.pl perl script and dumped to a file named
// dataset1.h.

#include "util.h"

#include "median.h"

//--------------------------------------------------------------------------
// Input/Reference Data

#include "dataset1.h"

//--------------------------------------------------------------------------
// Main

static short results_data[DATA_SIZE];

int main(int argc, char *argv[]) {

  // #if PREALLOCATE
  //   // If needed we preallocate everything in the caches
  //   median2(DATA_SIZE, input_data, results_data);
  // #endif

  // Do the filter
  setStats(1);
  median2(DATA_SIZE, input_data, results_data);
  setStats(0);

  int threadid = read_csr(mhartid);
  if (threadid == 0)
    printf("median done\n");

  // Check the results
  if (threadid == 0) {
    for (int i = 0; i < 20; i++) {
      for (int j = 0; j < 20; j++)
        printf("%d, ", results_data[i * 20 + j]);
      printf("\n");
    }
    int res = verifyShort(DATA_SIZE, results_data, verify_data);
    printf("verify done with %d\n", res);
    return res;
  } else {
    while (1)
      ;
  }
}

void thread_entry(int cid, int nc) {
  // while (cid != 0)
  //   ;
}
