#include "krig.h"

/*
 * 5th Kriging subroutine - produce the kriged estimate of value z
 * Parameters passed:	none
 */

/*
 * What this does: This function calculates the estimate of value z at the
 * cell location(east,west) being operated on by summing the product of
 * the Kriging weights in matrix x with the closest nsearch z values.
 */

double krig_z_est (void)
{
  int i, col;
  double sum_z;
  extern int nsearch;
  extern mat_struct *x, *smpl_z;

  sum_z = 0.0;

  for (i = 0; i < nsearch; i++)
  {
    sum_z += G_get_matrix_element(x, i, 1) * 
      G_get_matrix_element(smpl_z, i, 1);
  }
  if (sum_z < 0)
    sum_z = 0;

  return sum_z;
}
