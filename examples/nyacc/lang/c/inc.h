/* inc.h
 *
 * Copyright (C) 2015 Matthew R. Wette
 * 
 * Copying and distribution of this file, with or without modification,
 * are permitted in any medium without royalty provided the copyright
 * notice and this notice are preserved.  This file is offered as-is,
 * without any warranty.
 */
#ifndef __inc_h__
#define __inc_h__

typedef struct {
  int ix;           /* comment for ix */
  double c[4];      /* comment for c */
} ex1_t;

/* Initialize ex1 object. */
int ex1_init(void*);

#endif
