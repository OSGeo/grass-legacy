
/*
 * 5th Kriging subroutine - produce the kriged estimate of value z
 * Parameters passed:	none
 */

/*
 * What this does: This function calculates the estimate of value z at the
 * cell location(east,west) being operated on by summing the product of
 * the Kriging weights in matrix x with the closest nsearch z values.
 */

int krig_z_est (void)
{
  int i, col;
  int cell_value;
  double sum_z;
  extern int nsearch;
  extern float *x;
  extern double *smpl_z;
  cell_value = 0;

  sum_z = 0.0;

  for (i = 0; i < nsearch; i++)
  {
    sum_z += x[i + 1] * smpl_z[i];
    /*
     * if(sum_z > 10000) fprintf(stderr,"\nx[%d+1]=%lf
     * smpl_z[%d]=%lf",i,x[i+1],i,smpl_z[i]);
     */
  }
  if (sum_z < 0)
    sum_z = 0;
  cell_value = (int) (sum_z + 0.5);
  return cell_value;
}
