/* mlang.h 
 *
 * Copyright (C) 2018 Matthew R. Wette
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, see <http://www.gnu.org/licenses/>
 */
#ifndef mlang_h_
#define mlang_h_

typedef struct {
  int l;
  int *d;
} ml_ivec_t;

typedef struct {
  int l;
  double *d;
} ml_dvec_t;

typedef struct {
  int nr;
  int nc;
  double *d;
} ml_dmat_t;

#endif
/* --- last line --- */
