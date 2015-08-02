// inc.h
#ifndef __inc_h__
#define __inc_h__

typedef struct {
  int ix;           /* index */
  double c[4];      /* coefficient (deg/(volt^i))*/
  double oset;      /* offset (deg) */
} ex1_t;

/* Initialize ex1 object. */
int ex1_init(void*);

#endif
