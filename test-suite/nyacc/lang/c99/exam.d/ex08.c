#define assert(X) if (!(X)) abort()

int foo (int x) { assert (x == 1); }
