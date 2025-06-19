

typedef union {
  float a[9];
  long b[9];
} U;

float f(U x, int i) { return x.a[i]; }

long g(U x, int i) { return x.b[i]; }

