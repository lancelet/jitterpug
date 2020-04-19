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
#include <math.h>
#include <stdio.h>

/* ---- Stuff from the Pixar paper ---- */

typedef struct XY
{
  float x;
  float y;
} xy;

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

xy cmj(int s, int N, int p, float a) {
  int m = (int)(sqrtf(N * a));
  int n = (N + m - 1) / m;
  s = permute(s, N, p * 0x51633e2d); /* shuffle order */
  int sx = permute(s % m, m, p * 0x68bc21eb);
  int sy = permute(s / m, n, p * 0x02e5be93);
  float jx = randfloat(s, p * 0x967a889b);
  float jy = randfloat(s, p * 0x368cc8b7);
  xy r = {(sx + (sy + jx) / n) / m,
    (s + jy) / N};
  return r;
}

/* ---- Printing stuff out to use in Haskell tests ---- */

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

void printCmj5x3Example() {
  int s;
  xy p;
  float a = 5.0 / 3.0;
  printf("cmj, 5x3 pixel values:");
  for (s = 0; s < 5*3; ++s) {
    p = cmj(s, 5*3, 10, a);
    printf("(%f, %f)\n", p.x, p.y);
  }
}

int main() {

  printf("Permutation Examples:\n");
  printf("  For l = 8:\n");
  printPermuteExamples(8, 10, 0xa511e9b3);

  printf("\nrandfloat Examples:\n");
  printRandFloatExamples(0xa399d265, 10);
  printRandFloatExamples(0x711ad6a5, 10);

  printf("\nCMJ sample position examples:\n");
  printCmj5x3Example();

  return 0;
}
