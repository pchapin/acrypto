/*---------------------------------------------------------------------------
* FILE    : do_benchmarks.c
* SUBJECT : A benchmark program that uses OpenSSL's crypto library
* AUTHOR  : (C) Copyright 2008 by Peter C. Chapin
*
* This program is intended to mimic the structure of the ACO benchmark
* program and thus provide a point of comparision between ACO and a well
* known crypto library (OpenSSL's crypto library in this case).
*
* Please send comments or bug reports to
*
* Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------*/

// Standard
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

// OpenSSL
#include <openssl/blowfish.h>

#define WORKSPACE_SIZE (1024 * 1024)
#define PASS_COUNT      200

// ============
// Main Program
// ============

int main(int argc, char **argv)
{
  unsigned char  raw_key[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
  BF_KEY         key;
  unsigned char *workspace = (unsigned char *)malloc(WORKSPACE_SIZE);
  const int      block_count = WORKSPACE_SIZE/8;
  const int      workload = (PASS_COUNT * WORKSPACE_SIZE) / (1024 * 1024);
  int            i;                  // Loop index variable.
  int            j;                  // Loop index variable.
  int            result_okay = 1;    // =0 if the decrypted data is not correct.
  clock_t        start_time;         // Marks the time when computation began.
  clock_t        stop_time;          // Marks the time when computation ended.
  double         computation_time;   // The value stop_time - start_time as a double (seconds).

  // Do we have the memory we need?
  if (workspace == NULL) {
    printf("Unable to allocate workspace!\n");
    return 1;
  }

  // Fill the workspace with zeros.
  memset(workspace, 0, WORKSPACE_SIZE);

  // Prepare the key.
  BF_set_key(&key, 8, raw_key);

  printf("BLOWFISH benchmarks\n");
  printf("===================\n");

  start_time = clock();
  for (i = 0; i < PASS_COUNT; ++i) {
    for (j = 0; j < block_count; ++j) {
      unsigned char *current_block = workspace + (j << 3);
      BF_ecb_encrypt(current_block, current_block, &key, BF_ENCRYPT);
    }
  }
  stop_time = clock();
  computation_time = (double)(stop_time - start_time) / CLOCKS_PER_SEC;
  printf("Encryption rate = %f MiB/s\n", workload/computation_time);

  start_time = clock();
  for (i = 0; i < PASS_COUNT; ++i) {
    for (j = block_count; j > 0; --j) {
      unsigned char *current_block = workspace + ((j - 1) << 3);
      BF_ecb_encrypt(current_block, current_block, &key, BF_DECRYPT);
    }
  }
  stop_time = clock();
  computation_time = (double)(stop_time - start_time) / CLOCKS_PER_SEC;
  printf("Decryption rate = %f MiB/s\n", workload/computation_time);

  for (i = 0; i < WORKSPACE_SIZE; ++i) {
    if (workspace[i] != 0) result_okay = 0;
  }
  if (result_okay) {
    printf("PASSED!\n");
  }
  else {
    printf("FAILED!\n");
  }

  free(workspace);
  return 0;
}
