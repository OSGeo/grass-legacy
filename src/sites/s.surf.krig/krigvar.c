#include "krig.h"

/*
 * 6th Kriging subroutine -  produce the kriged estimate of the variance
 * of z Parameters passed: none What this does:	This function calculates
 * the variance of the estimate z at location(east, west) being operated
 * on by summing the product of the Kriging weights in matrix x with the
 * semivariance values of the distances to the nsearch closest values and
 * adding the returned lagrange value.
 */

double krig_var_est (void)
{
  int i;
  double sum_var_z;
  extern int nsearch;
  extern mat_struct *x, *sv_to_cell;


  sum_var_z = 0.0;
  for (i = 0; i < nsearch; i++)
    sum_var_z += G_matrix_get_element(x, i, 0) * 
      G_matrix_get_element(sv_to_cell, i, 0);
  if (sum_var_z < 0)
    sum_var_z = 0;

  return sum_var_z;
}
