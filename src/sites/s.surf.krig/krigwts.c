#include <stdlib.h>
#include <math.h>
#include "krig.h"
/* #include "dgesv.h" */

/*
4th Kriging subroutine - find the kriging weights through the
maniplulation of semivar. Matrices A . x = b Parameters passed: none
What this does: This function takes the semivariance values calculated
in the 3rd Kriging function and transfers them to matrices with the
appropriate lagrange parameters.  The matrices A and b are then passed
to the LAPACK routine DGESV to solve the linear algebra system to derive
matrix x, the Kriging weights.
 */

#define MATRIX_SZ 13

/*
#define f77_dgesv   dgesv_
*/


void krig_weights (void) {

  int i, j, k, n;
  int matdim, lda;
  double e, tmp = 0;
  extern int nsearch;
  extern mat_struct *x, *sv_to_cell, *sv_btw_smpl;
  mat_struct *C, *w;

  matdim = nsearch + 1;
  if( matdim % 2 == 1 ) lda = matdim + 1;
  else lda = matdim + 2;

  if( (C = G_matrix_init(matdim, matdim, lda)) == NULL ) {
    G_fatal_error("Unable to allocate coefficients matrix");
  }

  if( (w = G_matrix_init(matdim, 1, lda)) == NULL ) {
    G_fatal_error("Unable to allocate data matrix");
  }

  /*
  for (i = 0; i < matdim; i++)
  {
    G_matrix_set_element(w, i, 0, 0.0);
    G_matrix_set_element(x, i, 0, 0.0);    

    for (j = 0; j < matdim; j++)
    {
      G_matrix_set_element(C, i, j, 0.0);
    }
  }
  */

  /*
   * Given the list of distances for each point to the point being
   * estimated and calculate the semivariances according to the
   * appropriate model.  Matrix b.
   */

  for (i = 0; i < matdim - 1; i++)
  {
    G_matrix_set_element(w, i, 0, G_matrix_get_element(sv_to_cell, i, 0));
  }

  G_matrix_set_element(w, matdim - 1, 0, 1.0);

  /*
   * Organize the list of between_sample_distances into a 2D array
   * including the lagrange parameters. Matrix a.
   */

  for (i = 0; i < matdim - 2; i++)
  {
    for (j = i + 1; j < matdim - 1; j++)
    {
      G_matrix_set_element(C, i, j, G_matrix_get_element(sv_btw_smpl, i, j));
      G_matrix_set_element(C, j, i, G_matrix_get_element(sv_btw_smpl, i, j));
    }
  }

  for (i = 0; i < matdim; i++)
  {
    G_matrix_set_element(C, i, i, 0.0);
  }

  for( i = 0; i < matdim - 1; i++ ) {
    G_matrix_set_element(C, i, matdim - 1, 1.0);
    G_matrix_set_element(C, matdim - 1, i, 1.0);
  }

  
  /*
   * Now call LU matrix routines to solve for Matrix x. a . x = b This
   * subroutine returns the kriging weights solved through the LU matrix
   * system for soloving linear algebraic equations. The kriging weights
   * are stored in x[] and the lagrange parameter is storted in the x[n+1]
   * cell.
   */

  G_matrix_LU_solve(C, x, w, NONSYM);


  G_matrix_free(C);
  G_matrix_free(w);

}
