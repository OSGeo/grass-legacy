
/*
 * 6th Kriging subroutine -  produce the kriged estimate of the variance
 * of z Parameters passed: none What this does:	This function calculates
 * the variance of the estimate z at location(east, west) being operated
 * on by summing the product of the Kriging weights in matrix x with the
 * semivariance values of the distances to the nsearch closest values and
 * adding the returned lagrange value.
 */

int krig_var_est (void)
{
  int i;
  int cell_value;
  double sum_var_z, tmp;
  extern int nsearch;
  extern float *x, *sv_to_cell;


  sum_var_z = 0.0;
  for (i = 0; i < nsearch; i++)
    sum_var_z += x[i + 1] * sv_to_cell[i];
  sum_var_z += x[nsearch + 1];
  if (sum_var_z < 0)
    sum_var_z = 0;

  cell_value = (int) (sum_var_z + 0.5);

  return cell_value;
}
