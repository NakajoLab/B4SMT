// See LICENSE for license details.

//**************************************************************************
// Median filters
//--------------------------------------------------------------------------

#define DTYPE unsigned char
#define CORE_COUNT 4

void median(int n, DTYPE input[], DTYPE results[]);

// Simple assembly version
void median_asm(int n, DTYPE input[], DTYPE results[]);
