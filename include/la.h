/******************************************************************************
 * la.h
 * wrapper modules for linear algebra problems
 * Structures, definitions and prototypes.
 * linking to BLAS / LAPACK (and others?)

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 26th. Sep. 2000
 * Last updated 27th. Sep. 2000
 *

 * This file is part of GRASS GIS. It is free software. You can 
 * redistribute it and/or modify it under the terms of 
 * the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option)
 * any later version.
 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 ******************************************************************************/

#ifndef LA_H_
#define LA_H_

#include <g2c.h>
/* QUESTION: On some systems there appears to be no default link
   to this. Do we create a symlink to
   /usr/lib/gcc-lib/<platform>/<vers_num>/include/g2c.h

   or link to any old f2c.h that happens to hanging around?

   A job for autoconf

   [Also a consideration for -lg2c]
*/

/* The following may have to be selectively installed according
   to platform, at least partly
*/

#include "blas.h"
#include "lapack.h"


/* define macros for fortran symbols (called directly). Needed because 
   of platform invariance on fortran->C symbol translations
*/

#define f77_dgesv                   dgesv_
#define f77_dgemm                   dgemm_

/* Operations should know type of coefficient matrix, so that
   they can call the right driver
*/

typedef enum { NONSYM, SYM, HERMITIAN } mat_type;



/************************************************************
 *                                                          *
 * A general matrix wrapper for use with BLAS / LAPACK      *
 *  routines, and perhaps elsewhere                         *
 *                                                          *
 ************************************************************/

typedef struct matrix_ {


  int rows, cols;    /* Rows and columns of matrix */
  int ldim;          /* Lead dimension of matrix.
			How many `rows' are alloc'ed? May exceed
			the real number of rows `rows'
		     */
  doublereal *vals;  /* The values (should be dimensioned to
			lda * cols
		     */
  int is_init;       /* Is this matrix initialised: values array
			is allocated and parameters set ?
		     */
}  mat_struct;


/* Prototypes */

mat_struct *G_matrix_init(int, int, int);
int G_matrix_set(mat_struct *, int, int, int);
mat_struct *G_matrix_add(mat_struct *, mat_struct *);
mat_struct *G_matrix_product(mat_struct *, mat_struct *);
int G_LU_solve(mat_struct *, mat_struct *, mat_struct *, mat_type);
mat_struct *G_matrix_inverse(mat_struct *);
void G_matrix_free(mat_struct *);
int G_set_matrix_element(mat_struct *, int, int, double);
double G_get_matrix_element(mat_struct *, int, int);

#endif /* LA_H_ */

