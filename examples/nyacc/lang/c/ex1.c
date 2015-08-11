// ex1.c
#include "inc.h"

int ex1_foo(ex1_t *mod, double t, double *x, int *y) {
   int i = mod->xyz.i;
   ++i;
   return i+2;
}

int x; // this is x 
/* this is lone comment */

/* --- last line */
