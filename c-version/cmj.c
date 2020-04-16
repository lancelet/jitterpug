/* This is code transcribed from:
 *
 *   Kensler, A (2013) Correlated Multi-Jittered Sampling
 *     Pixar Technical Memo 13-01.
 *
 * Its purpose is to compare output against the Haskell version, which we
 * expect to produce bit-for-bit identical output.
 *
 * Run using:
 *   make
 *   ./cmj
 */
#include <stdio.h>

unsigned permute(unsigned i, unsigned l, unsigned p) {
  unsigned w = l - 1;
  w |= w >> 1;
  w |= w >> 2;
  w |= w >> 4;
  w |= w >> 8;
  w |= w >> 16;
  do {
    i ^= p;              i *= 0xe170893d;
    i ^= p       >> 16;
    i ^= (i & w) >>  4;
    i ^= p       >>  8;  i *= 0x0929eb3f;
    i ^= p       >> 23;
    i ^= (i & w) >>  1;  i *= 1 | p >> 27;
                         i *= 0x6935fa69;
    i ^= (i & w) >> 11;  i *= 0x74dcb303;
    i ^= (i & w) >>  2;  i *= 0x9e501cc3;
    i ^= (i & w) >>  2;  i *= 0xc860a3df;
    i &= w;
    i ^= i       >>  5;
  } while (i >= l);
  return (i + p) % l;
}

float randfloat(unsigned i, unsigned p) {
  i ^= p;
  i ^= i >> 17;
  i ^= i >> 10;     i *= 0xb36534e5;
  i ^= i >> 12;
  i ^= i >> 21;     i *= 0x93fc4795;
  i ^= 0xdf6e307f;
  i ^= i >> 17;     i *= 1 | p >> 18;
  return i * (1.0f / 4294967808.0f);
}

void printPermute(unsigned l, unsigned p) {
  unsigned i, result;
  printf("[");
  for (i = 0; i < l; ++i) {
    result = permute(i, l, p);
    printf("%d", result);
    if (i < l - 1) {
      printf(", ");
    }
  }
  printf("]");
}

void printPermuteExamples(unsigned l, unsigned nCases, unsigned pOffset) {
  unsigned p;
  for (p = pOffset; p < (pOffset + nCases); ++p) {
    printf("p = %#x: ", p);
    printPermute(l, p);
    printf("\n");
  }
}

void printRandFloatExamples(unsigned p, unsigned nCases) {
  unsigned i;
  float result;
  printf("p = %#x: ", p);
  printf("[");
  for (i = 0; i < nCases; ++i) {
    result = randfloat(i, p);
    printf("%f", result);
    if (i < nCases - 1) {
      printf(", ");
    }
  }
  printf("]\n");
  printf("Casting float to unsigned:\n");
  printf("[");
  for (i = 0; i < nCases; ++i) {
    result = randfloat(i, p);
    printf("%#x", *(unsigned*)&result);
    if (i < nCases - 1) {
      printf(", ");
    }
  }
  printf("]\n");
}

int main() {

  printf("Permutation Examples:\n");
  printf("  For l = 8:\n");
  printPermuteExamples(8, 10, 0xa511e9b3);

  printf("\nrandfloat Examples:\n");
  printRandFloatExamples(0xa399d265, 10);
  printRandFloatExamples(0x711ad6a5, 10);

  return 0;
}
