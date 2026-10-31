// See LICENSE for license details.

//**************************************************************************
// Median filters
//--------------------------------------------------------------------------

#define DTYPE short
#define CORE_COUNT 4

// Simple C version
void median1(int n, DTYPE input[], DTYPE results[]);

// C with multi-threading version
void median2(int n, DTYPE input[], DTYPE results[]);

// C with P-Extension version
void median3(int n, DTYPE input[], DTYPE results[]);

// C with P-Extension and threading version
void median4(int n, DTYPE input[], DTYPE results[]);

// Simple assembly version
void median_asm(int n, DTYPE input[], DTYPE results[]);
