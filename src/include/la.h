/******************************************************************************
 * la.h
 * wrapper modules for linear algebra problems
 * Structures, definitions and prototypes.
 * linking to BLAS / LAPACK (and others?)

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 26th. Sep. 2000
 * Last updated 28th. Sep. 2000
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



/* Useful defines */

#define MAX_POS          1   /* Indicates maximum value         */
#define MAX_NEG         -1   /* Indicates minimum value         */
#define MAX_ABS          0   /* Indicates absolute value        */

#define DO_COMPACT       0   /* Elliminate unnecessary rows (cols) in matrix  */
#define NO_COMPACT       1   /* ... or not                                    */


/* define macros for fortran symbols (called directly). Needed because 
   of platform invariance on fortran->C symbol translations
*/

#define f77_dgesv                   dgesv_
#define f77_dgemm                   dgemm_
#define f77_dnrm2                   dnrm2_

/* Operations should know type of coefficient matrix, so that
   they can call the right driver
*/

typedef enum { NONSYM, SYM, HERMITIAN } mat_type;
typedef enum { MATRIX_, ROWVEC_, COLVEC_ } mat_spec;
typedef enum { RVEC, CVEC }             vtype;



/************************************************************
 *                                                          *
 * A general matrix wrapper for use with BLAS / LAPACK      *
 *  routines, and perhaps elsewhere                         *
 *                                                          *
 ************************************************************/

typedef struct matrix_ {

  mat_spec type;      /* Is it doing duty as a matrix, row vector or
			 column vector?
		      */

  int v_indx;         /* In the event this is serving as a vector, which
			 row(column) is active?  If a matrix this is ignored.
			 If the value is <0, the first row(column) is
			 assumed, ie. index 0.
		      */

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

typedef mat_struct vec_struct;



/* Prototypes */

/* Matrix routines corresponding to BLAS Level III */

mat_struct *G_matrix_init(int, int, int);
int G_matrix_set(mat_struct *, int, int, int);
mat_struct *G_matrix_copy(const mat_struct *);
mat_struct *G_matrix_add(mat_struct *, mat_struct *);
mat_struct *G_matrix_subtract(mat_struct *, mat_struct *);
mat_struct *G_matrix_scale(mat_struct *, const double);
mat_struct *G__matrix_add(mat_struct *, mat_struct *, const double, const double);
mat_struct *G_matrix_product(mat_struct *, mat_struct *);
mat_struct *G_matrix_transpose(mat_struct *);
int G_matrix_LU_solve(const mat_struct *, mat_struct **, const mat_struct *, mat_type);
mat_struct *G_matrix_inverse(mat_struct *);
void G_matrix_free(mat_struct *);
void G_matrix_print(mat_struct *);
int G_matrix_set_element(mat_struct *, int, int, double);
double G_matrix_get_element(mat_struct *, int, int);


/* Matrix-vector routines corresponding to BLAS Level II */

vec_struct *G_matvect_get_column(mat_struct *, int);
vec_struct *G_matvect_get_row(mat_struct *, int);
int G_matvect_extract_vector( mat_struct *, vtype, int);
int G_matvect_retrieve_matrix(vec_struct *);


/* Vector routines corresponding to BLAS Level I */

vec_struct *G_vector_init(int, int, vtype);
int G_vector_set(vec_struct *, int, int, vtype, int);
double G_vector_norm_euclid(vec_struct *);
double G_vector_norm_maxval(vec_struct *, int);
vec_struct *G_vector_copy(const vec_struct *, int);

#endif /* LA_H_ */


