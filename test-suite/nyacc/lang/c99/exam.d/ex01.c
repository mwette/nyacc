// ex1.c
#ifdef __cplusplus__
extern "C" {
#endif
#include "inc.h"

#define A 1
#define B(X) ((X)+1)

#ifdef A
int y;
#elif defined(B) /* check */
double y;
#else /* else */
#error "foo"
#endif /* def A */

eval_t x;
struct foo;

int d = 0x123; /* d comment */
float f = 0.0; 
const volatile int * const * foo = 0;

#define OFFSET(T,V) (((T*)0)->V)

typedef struct {
  /* hello */
  eval_t x; /* comment */
  int y;
} xyz_t;

typedef struct zippy {
  double z[2];
} zip_t;

int foo(int y) {
  double d;

  if (y > 0) {
    d = +1.0;
  } else if (y == 0) {
    d = 0.0;
  } else {
    d = -1.0;
  }
  return 1;
}

/* this is lone comment */
#ifdef __cplusplus__
}
#endif
/* --- last line --- */
