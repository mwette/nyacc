/* ex1.c
 *
 * Copyright (C) 2015 Matthew R. Wette
 * 
 * Copying and distribution of this file, with or without modification,
 * are permitted in any medium without royalty provided the copyright
 * notice and this notice are preserved.  This file is offered as-is,
 * without any warranty.
 */
#include "inc.h"

int ex1_foo(ex1_t *mod, double t, double *x, int *y) {
   int i = mod->xyz.i;
   ++i;
   return i+2;
}

int x; // this is x 
/* this is lone comment */

/* --- last line */
