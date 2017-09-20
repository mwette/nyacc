// ex1.c
#ifdef __cplusplus__
extern "C" {
#endif
#include "inc.h"

#define A 1

#ifdef A
int y;
#elif defined(B)
double y;
#else
#error "foo"
#endif

eval_t x;
struct foo;

int d = 0x123; /* d comment */
float f = 0.0; 

#define OFFSET(T,V) (((T*)0)->V)

typedef struct {
  /* hello */
  eval_t x; /* comment */
  int y;
} xyz_t;

int foo(int y) {
  int i, j, k;

  for (j = 1, k = 2, i = 0; i < 10; i++) {
    j = j + 1;
  }
  if (y > 0) {
    k = +1;
  } else if (y == 0) {
    k = OFFSET(xyz_t,y);
  } else {
    k = -1;
  }
  return 1 + x->foo(k + y);
}

/* this is lone comment */
#ifdef __cplusplus__
}
#endif
/* --- last line --- */
