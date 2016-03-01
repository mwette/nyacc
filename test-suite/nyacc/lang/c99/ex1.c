// ex1.c
#ifdef __cplusplus__
extern "C" {
#endif
#include "inc.h"

eval_t x;
struct foo;

int d = 0x123; /* d comment */
float f = 0.0; 

#define OFFSET(T,V) (((T*)0)->V)

typedef struct {
  /* hello */
  eval_t x; /* comment */
  int x;
} xyz_t;

int foo(int y) {
  double d;

  d = 0.0;
}

/* this is lone comment */
#ifdef __cplusplus__
}
#endif
/* --- last line --- */
