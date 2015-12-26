// ex1.c
#ifdef __cplusplus__
extern "C" {
#endif
#include "inc.h"

eval_t x;
struct foo;

int d = 0x123;

#define OFFSET(T,V) (((T*)0)->V)

typedef struct {
  /* hello */
  eval_t x; /* comment */
  int x;
  /* world */
} xyz_t;

int y = (int)(((xyz_t*)0)->x);
int x;
/* this is lone comment */
#ifdef __cplusplus__
}
#endif
/* --- last line --- */
