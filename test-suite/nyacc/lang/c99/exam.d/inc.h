// inc.h
#ifndef __inc_h__
#define __inc_h__

#ifdef A
typedef enum { EA_ZERO = 0, EA_ONE, EA_TWO = 2 } eval_t;
#elif defined(B)
typedef enum { EB_ZERO = 0, EB_ONE, EB_TWO = 2 } eval_t;
#else
typedef enum { EC_ZERO = 0, EC_ONE, EC_TWO = 2 } eval_t;
#endif

typedef enum {
 ZZ_ZERO = 0,  /* comment */
 ZZ_ONE, 
 ZZ_TWO = 2 
} zz_t;

typedef struct {
  int ix;           /* comment for ix */
  double c[4];      /* comment for c */
} ex1_t;

/* Initialize ex1 object. */
int ex1_init(void*);

#endif /* last line */
