/******************************************************************************
 * la.c
 * wrapper modules for linear algebra problems
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


#include <stdlib.h>
#include <stdio.h>
#include "la.h"

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */


/************************************************************
 *                                                          *
 * G_matrix_init()                                          *
 *                                                          *
 * Initialise a matrix structure                            *
 *                                                          *
 ************************************************************/

mat_struct *
G_matrix_init(int rows, int cols, int ldim ) {

  mat_struct *tmp_arry;

  if( rows < 1 || cols < 1 || ldim < rows ) {
    fprintf(stderr, "Error: Matrix dimensions out of range\n");
    return NULL;
  }

  tmp_arry = (mat_struct *)G_malloc( sizeof(mat_struct) );
  tmp_arry->rows = rows;
  tmp_arry->cols = cols;
  tmp_arry->ldim = ldim;
  tmp_arry->type = MATRIX_;
  tmp_arry->v_indx = -1;
  
  tmp_arry->vals = (doublereal *)G_calloc( ldim * cols,
					   sizeof(doublereal) );
  tmp_arry->is_init = 1;

  return tmp_arry;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/************************************************************
 *                                                          *
 * G_matrix_set()                                           *
 *                                                          *
 * Set parameters for a matrix structure that is            *
 * allocated but not yet initialised fully                  *
 *                                                          *
 ************************************************************/

int
G_matrix_set(mat_struct *A, int rows, int cols, int ldim) {

  if( rows < 1 || cols < 1 || ldim < 0 ) {
    fprintf(stderr, "Error: Matrix dimensions out of range\n");
    return -1;
  }

  A->rows = rows;
  A->cols = cols;
  A->ldim = ldim;
  A->type = MATRIX_;
  A->v_indx = -1;
  
  A->vals = (doublereal *)G_calloc( ldim * cols,
					   sizeof(doublereal) );
  A->is_init = 1;

  return 0;
  
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */



/************************************************************
 *                                                          *
 * G_matrix_add()                                           *
 *                                                          *
 * Add / multiply two matrices and return the result        *
 * The receiving structure should not be                    *
 * initialised, as the matrix is created by the routine     *
 *                                                          *
 ************************************************************/


mat_struct *
G_matrix_add(mat_struct *mt1, mat_struct *mt2) {

  mat_struct *mt3;
  int i, j; /* loop variables */

  if( (mt1->is_init) || (mt2->is_init) ) {
    fprintf(stderr, "Error: One or both input matrices uninitialised\n");
    return NULL;
  }

  if(mt1->rows != mt2->rows || mt1->cols != mt2->cols) {
    fprintf(stderr, "Error: Matrix order does not match\n");
    return NULL;
  }

  if( (mt3 = G_matrix_init(mt1->rows, mt1->cols, mt1->ldim)) == NULL ) {
    fprintf(stderr, "Unable to allocate space for matrix sum\n");
    return NULL;
  }

  for( i = 0; i < mt3->rows; i++ ) {
    for( j = 0; j < mt3->cols; j++) {
      mt3->vals[i + mt3->ldim * j] = mt1->vals[i + mt1->ldim * j] +
	mt2->vals[i + mt2->ldim * j];
    }
  }

  return mt3;
}


mat_struct *
G_matrix_product(mat_struct *mt1, mat_struct *mt2) {

  mat_struct *mt3;
  doublereal unity = 1, zero = 0;
  integer rows, cols, interdim, lda, ldb;
  integer1 no_trans = 'n';

  if( (mt1->is_init) || (mt2->is_init) ) {
    fprintf(stderr, "Error: One or both input matrices uninitialised\n");
    return NULL;
  }

  if(mt1->cols != mt2->rows) {
    fprintf(stderr, "Error: Matrix order does not match\n");
    return NULL;
  }

  if( (mt3 = G_matrix_init(mt1->rows, mt2->cols, mt1->ldim)) == NULL ) {
    fprintf(stderr, "Unable to allocate space for matrix product\n");
    return NULL;
  }

  /* Call the driver */

  rows = (integer)mt1->rows;
  interdim = (integer)mt1->cols;
  cols = (integer)mt2->cols;

  lda = (integer)mt1->ldim;
  ldb = (integer)mt2->ldim;

  f77_dgemm(&no_trans, &no_trans, &rows, &cols, &interdim, &unity, mt1->vals,
	    &lda, mt2->vals, &ldb, &zero, mt3->vals, &lda);

  return mt3;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */



/************************************************************
 *                                                          *
 * G_LU_solve()                                             *
 *                                                          *
 * Solve a general system A.X=B, where A is a NxN matrix,   *
 * X and B are NxC matrices, and we are to solve for C      *
 * arrays in X given B. Uses LU decomposition.              *
 *                                                          *
 * Links to LAPACK function dgesv_() and similar to perform *
 * the core routine. (By default solves for a general       *
 * non-symmetric matrix.                                    *
 *                                                          *
 ************************************************************/

/*** NOT YET COMPLETE: only some solutions' options available ***/

int
G_matrix_LU_solve(mat_struct *mt1, mat_struct *xmat, mat_struct *bmat,
		  mat_type mtype) {

  int i; /* loop */
  mat_struct *wmat;  /* ptr to working matrix */

  if(mt1->is_init == 0 || bmat->is_init == 0) {
    fprintf(stderr, "Input error: one or both data matrices uninitialised\n");
    return -1;
  }

  if(mt1->rows != mt1->cols || mt1->rows < 1 || mt1->cols < 1) {
    fprintf(stderr, "Principal matrix is not properly dimensioned\n");
    return -1;
  }

  if(mt1->cols != xmat->rows || mt1->cols != bmat->rows ||
     xmat->cols != bmat->cols) {
    fprintf(stderr, "Input matrices' dimensions are not in sync\n");
    return -1;
  }

  if(bmat->cols < 1) {
    fprintf(stderr, "Input error: you must have at least one array to solve\n");
    return -1;
  }

  /* Now initialise the solution matrix if it has not aleady been */

  if(xmat->is_init == 0) {
    if( G_matrix_set(xmat, bmat->rows, bmat->cols, bmat->ldim) < 0 ) {
      fprintf(stderr, "Could not allocate space for solution matrix\n");
      return -1;
    }
  }

  if(xmat->rows != bmat->rows || xmat->cols !=bmat->cols) {
    fprintf(stderr, "Input error: solution matrix does not match data matrix\n");
    return -1;
  }

  /* Copy the contents of the data matrix, to preserve the
     original information 
  */

  if( (wmat = G_matrix_init(bmat->rows, bmat->cols, bmat->ldim)) == NULL ) {
    fprintf(stderr, "Could not allocate space for working matrix\n");
    return -1;
  }

  wmat->rows = bmat->rows;
  wmat->cols = bmat->cols;
  wmat->ldim = bmat->ldim;
  for( i = 0; i < bmat->cols * bmat->ldim; i++ ) {
    wmat->vals[i] = bmat->vals[i];
  }
  wmat->is_init = 1;

  /* Now call appropriate LA driver to solve equations */

  switch(mtype) {

  case NONSYM:
    {
      integer *perm, res_info;
      int indx; /* loop variable */
      integer num_eqns, nrhs, lda, ldb;

      perm = (integer *)G_malloc(bmat->ldim);

      /* Set fields to pass to fortran routine */
      num_eqns = (integer)mt1->rows;
      nrhs = (integer)xmat->cols;
      lda = (integer)mt1->ldim;
      ldb = (integer)bmat->ldim;

      /* Call LA driver */
      f77_dgesv(&num_eqns, &nrhs, mt1->vals, &lda, perm, bmat->vals,
		&ldb, &res_info);

      /* Copy the results from the modified data matrix, taking account of pivot
	 permutations
      */
      
      for(indx = 0; indx < bmat->rows; indx++)
	xmat->vals[indx] = bmat->vals[perm[indx]];

      /* Free temp arrays */
      G_free(perm);
      G_matrix_free(wmat);

      if( res_info > 0 ) {
	fprintf(stderr, "Error: matrix (or submatrix is singular). Solution undetermined\n");
	return 1;
      }

      else if( res_info < 0 ) {
	fprintf(stderr, "Error in LA routine.\n");
	return -1;	
      }
      break;
    }
  default:
    {
      fprintf(stderr, "Procedure not yet available for selected matrix type\n");
      return -1;
    }
  }  /* end switch */

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */




/************************************************************
 *                                                          *
 * G_matrix_inverse()                                       *
 *                                                          *
 * Calls G_matrix_LU_solve() to obtain matrix inverse using *
 * LU decomposition. Returns NULL on failure.               *
 *                                                          *
 ************************************************************/


mat_struct *
G_matrix_inverse(mat_struct *mt) {

  mat_struct *mt0, *res;
  int i, j, k; /* loop */

  if(mt->rows != mt->cols) {
      fprintf(stderr, "Error matrix is not square. Cannot determine inverse\n");
      return NULL;    
  }

  if( (mt0 = G_matrix_init(mt->rows, mt->rows, mt->ldim)) == NULL ) {
      fprintf(stderr, "Unable to allocate space for matrix\n");
      return NULL;    
  }

  if( (res = G_matrix_init(mt->rows, mt->rows, mt->ldim)) == NULL ) {
      fprintf(stderr, "Unable to allocate space for matrix\n");
      return NULL;    
  }

  /* Set `B' matrix to unit matrix */

  for( i = 0; i < mt0->rows - 1; i++ ) {
    mt0->vals[i + i * mt0->ldim] = 1.0;

    for( j = i + 1; j < mt0->cols; j++ ) {
      mt0->vals[i + j * mt0->ldim] = mt0->vals[j + i * mt0->ldim] = 0.0;
    }
  }

  mt0->vals[mt0->rows - 1 + (mt0->rows - 1) * mt0->ldim] = 1.0;

  /* Solve system */

  if( (k = G_matrix_LU_solve(mt, res, mt0, NONSYM)) == 1 ) {
      fprintf(stderr, "Error: matrix is singular\n");
      G_matrix_free(mt0);
      return NULL;        
  }

  else if( k < 0 ) {
      fprintf(stderr, "Error in LA procedure.\n");
      G_matrix_free(mt0);
      return NULL;        
  }

  else {
    G_matrix_free(mt0);
    return res;
  }

}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */




/************************************************************
 *                                                          *
 * G_matrix_free()                                          *
 *                                                          *
 * Free up allocated matrix                                 *
 *                                                          *
 ************************************************************/


void
G_matrix_free(mat_struct *mt) {

  if(mt->is_init)
    G_free(mt->vals);

  G_free(mt);
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */




/************************************************************
 *                                                          *
 * G_matrix_set_element()                                   *
 *                                                          *
 * Set the value of the (i,j)th element to a double         *
 * value. Index values are C-like ie. zero-based            *
 *                                                          *
 ************************************************************/


int
G_matrix_set_element(mat_struct *mt, int rowval, int colval,
		     double val) {

  if(!mt->is_init) {
    fprintf(stderr, "Error: element array has not been allocated\n");
    return -1;
  }

  if(rowval >= mt->rows || colval >= mt->cols ||
     rowval < 0 || colval < 0) {
    fprintf(stderr, "Error: specified element is outside array bounds\n");
    return -1;
  }

  mt->vals[rowval + colval * mt->ldim] = (doublereal)val;

  return 0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */




/************************************************************
 *                                                          *
 * G_matrix_get_element()                                   *
 *                                                          *
 * Retrieve the value of the (i,j)th element to a double    *
 * value. Index values are C-like ie. zero-based            *
 *                                                          *
 ************************************************************/


double
G_matrix_get_element(mat_struct *mt, int rowval, int colval) {

  /* Should do some checks, but this would require an error control
     system: later?
  */

  double val;

  return val = (double)mt->vals[rowval + colval * mt->ldim];

}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */






/************************************************************
 *                                                          *
 * G_matvect_get_column()                                   *
 *                                                          *
 * Retrieve a column  of the matrix to a vector structure   *
 *                                                          *
 ************************************************************/

vec_struct *
G_matvect_get_column(mat_struct *mt, int col) {

  int i; /* loop */
  vec_struct *vc1;

  if(col < 0 || col >= mt->cols) {
    fprintf(stderr, "Specified matrix column index is outside range\n");
    return NULL;
  }

  if(!mt->is_init) {
    fprintf(stderr, "Error: matrix is not initialised\n");
    return NULL;
  }

  if( (vc1 = G_vector_init(mt->rows, mt->ldim, CVEC)) == NULL ) {
    fprintf(stderr, "Couldn't allocate space for vector structure\n");
    return NULL;
  }

  for ( i = 0; i < mt->rows; i++ )
    G_matrix_set_element( (mat_struct *)vc1, i, 0, 
			  G_matrix_get_element(mt, i, col) );

  return vc1;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */



/************************************************************
 *                                                          *
 * G_vector_init()                                          *
 *                                                          *
 * Initialise a vector structure                            *
 *                                                          *
 ************************************************************/

vec_struct *
G_vector_init(int cells, int ldim, vtype vt) {

  vec_struct *tmp_arry;

  if( (cells < 1) || (vt == RVEC && ldim < 1)
      || (vt == CVEC && ldim < cells) || ldim < 0 ) {
    fprintf(stderr, "Error: Vector dimensions out of range\n");
    return NULL;
  }

  tmp_arry = (vec_struct *)G_malloc( sizeof(vec_struct) );

  if(vt = RVEC) {
    tmp_arry->rows = 1;
    tmp_arry->cols = cells;
    tmp_arry->ldim = ldim;
    tmp_arry->type = ROWVEC_;
  }

  else if(vt = CVEC) {
    tmp_arry->rows = cells;
    tmp_arry->cols = 1;
    tmp_arry->ldim = ldim;
    tmp_arry->type = COLVEC_;
  }

  tmp_arry->v_indx = -1;
  
  tmp_arry->vals = (doublereal *)G_calloc( ldim * tmp_arry->cols,
					   sizeof(doublereal) );
  tmp_arry->is_init = 1;

  return tmp_arry;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/************************************************************
 *                                                          *
 * G_vector_set()                                           *
 *                                                          *
 * Set parameters for a vector structure that is            *
 * allocated but not yet initialised fully                  *
 *                                                          *
 ************************************************************/

int
G_vector_set(vec_struct *A, int cells, int ldim, vtype vt) {

  if( (cells < 1) || (vt == RVEC && ldim < 1)
      || (vt == CVEC && ldim < cells) || ldim < 0 ) {
    fprintf(stderr, "Error: Vector dimensions out of range\n");
    return -1;
  }

  if (vt == RVEC) {
    A->rows = 1;
    A->cols = cells;
    A->ldim = ldim;
    A->type = ROWVEC_;
  }
  else {
    A->rows = cells;
    A->cols = 1;
    A->ldim = ldim;
    A->type = COLVEC_;
  }

  A->v_indx = -1;
  
  A->vals = (doublereal *)G_calloc( ldim * A->cols,
					   sizeof(doublereal) );
  A->is_init = 1;

  return 0;
  
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */




/************************************************************
 *                                                          *
 * G_vector_norm_euclid()                                   *
 *                                                          *
 * Calculates the euclidean norm of a row or column         *
 * vector, using BLAS routine dnrm2_()                      *
 *                                                          *
 ************************************************************/

double
G_vector_norm_euclid(vec_struct *vc) {

  integer incr, Nval;
  doublereal *startpt;

  if(!vc->is_init) {
    fprintf(stderr, "Error: matrix is not initialised\n");
    exit(-1);
  }


  if(vc->type == ROWVEC_) {
    Nval = (integer)vc->cols;
    incr = (integer)vc->ldim;
    if(vc->v_indx < 0)
      startpt = vc->vals;
    else
      startpt = vc->vals + vc->v_indx;
  }
  else {
    Nval = (integer)vc->rows;
    incr = 1;
    if(vc->v_indx < 0)
      startpt = vc->vals;
    else
      startpt = vc->vals + vc->v_indx * vc->ldim;
  }

  /* Call the BLAS routine dnrm2_() */

  return (double)f77_dnrm2(&Nval, startpt, &incr);

}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */




/************************************************************
 *                                                          *
 * G_vector_norm_maxval()                                   *
 *                                                          *
 * Calculates the maximum value of a row or column          *
 * vector.                                                  *
 *                                                          *
 ************************************************************/

double
G_vector_norm_maxval(vec_struct *vc, int vflag) {

  doublereal xval, *startpt, *curpt;
  double cellval;
  int ix;       /* loop */
  int ncells, incr, cnt;

  if(!vc->is_init) {
    fprintf(stderr, "Error: matrix is not initialised\n");
    exit(-1);
  }

  if(vc->type == ROWVEC_) {
    ncells = (integer)vc->cols;
    incr = (integer)vc->ldim;
    if(vc->v_indx < 0)
      startpt = vc->vals;
    else
      startpt = vc->vals + vc->v_indx;
  }
  else {
    ncells = (integer)vc->rows;
    incr = 1;
    if(vc->v_indx < 0)
      startpt = vc->vals;
    else
      startpt = vc->vals + vc->v_indx * vc->ldim;
  }

  xval = *startpt;
  curpt = startpt;

  while(ncells > 0) {
    if(curpt != startpt) {
      switch(vflag) {

      case MAX_POS:
	{
	  if(*curpt > xval) xval = *curpt;
	  break;
	}

      case MAX_NEG:
	{
	  if(*curpt < xval) xval = *curpt;
	  break;
	}

      case MAX_ABS:
	{
	  cellval = (double)(*curpt);
	  if(hypot(cellval, cellval) > (double)xval)
	    xval = *curpt;
	}


      }  /* switch */
    }  /* if(curpt != startpt) */

    curpt += incr;
    ncells--;
  }

  return (double)xval;
}



