#include "nrutil.h"
#include "krig.h"
/*
4th Kriging subroutine - find the kriging weights through the
maniplulation of semivar. Matrices A . x = b Parameters passed: none
What this does: This function takes the semivariance values calculated
in the 3rd Kriging function and transfers them to matrices with the
appropriate lagrange parameters.  The matrices A and b are then passed
to the numerical recipes routines which invert matrix A multiply it
with b to derive matrix x, the Kriging weights.
 */

#define MATRIX_SZ 13

void krig_weights (void)
{
  int i, j, k, *index, n;
  float e, *w, **C, tmp = 0;
extern int nsearch;
extern float *x, *sv_to_cell, **sv_btw_smpl;

  /* Dynamically dimension 2 vectors and 1 matrix */

  C = matrix (1, nsearch + 1, 1, nsearch + 1);
  w = vector (1, nsearch + 1);
  index = ivector (1, nsearch + 1);

  for (i = 1; i <= nsearch + 1; i++)
  {
    w[i] = 0.0;
    x[i] = 0.0;
    for (j = 1; j <= nsearch + 1; j++)
    {
      C[i][j] = 0.0;
    }
  }

  /* Set Lagrange Parameters in w[vector] and a[matrix][] */

  w[nsearch + 1] = 1.0;
  for (i = 1; i <= nsearch; i++)
    C[i][nsearch + 1] = 1.0;

  /*
   * Given the list of distances for each point to the point being
   * estimated and calculate the semivariances according to the
   * appropriate model.  Matrix b.
   */

  for (i = 0; i < nsearch; i++)
  {
    w[i + 1] = sv_to_cell[i];
  }

  /*
   * Organize the list of between_sample_distances into a 2D array
   * including the lagrange parameters. Matrix a.
   */

  for (i = 0; i < nsearch; i++)
  {
    for (j = i + 1; j < nsearch; j++)
    {
      C[i + 1][j + 1] = sv_btw_smpl[i][j];
    }
  }

  for (i = 1; i <= nsearch; i++)
  {
    for (j = 1; j <= nsearch + 1; j++)
    {
      if (i == j)
	C[i][j] = 0.0;
      else
	C[j][i] = C[i][j];
    }
  }

  /*
   * Now call LU matrix routines to solve for Matrix x. a . x = b This
   * subroutine returns the kriging weights solved through the LU matrix
   * system for soloving linear algebraic equations. The kriging weights
   * are stored in x[] and the lagrange parameter is storted in the x[n+1]
   * cell.
   */

  ludcmp (C, nsearch + 1, index, &e);	/* matrix algebra routines from */
  lubksb (C, nsearch + 1, index, w);	/* numerical recipies           */

  for (i = 1; i <= nsearch + 1; i++)
    x[i] = w[i];		/* read in weights to x[]  */

/*
  free_vector (w, 1, nsearch + 1);
  free_matrix (C, 1, nsearch + 1, 1, nsearch + 1);
*/
  return;
}

