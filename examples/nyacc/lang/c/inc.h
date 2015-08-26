// inc.h
#ifndef __inc_h__
#define __inc_h__

typedef enum { EV_ZERO = 0, EV_ONE, EV_TWO = 2 } eval_t;

typedef struct {
  int ix;           /* comment for ix */
  double c[4];      /* comment for c */
} ex1_t;

/* Initialize ex1 object. */
int ex1_init(void*);

#endif
