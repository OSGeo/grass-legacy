
/*
 * 3rd Kriging subroutine - find semivariance from variogram model for the
 * calculated distances
 */
/*-
  Parameters passed:
  model_type		variogram model needed to calculate weights(char)
  range		model range - distance(double)
  sill			model sill - max theoretical variance(double)
  nugget		model nugget variance - unresolable variance
/*

/*  What this does:
This function provides a "modelled" variogram based on the shape, range,
sill and nugget determined from the examination of the experimental
variogram.  Semivariance values are calculated accordingly.	*/
#include <math.h>

void variogram_model (int model_num, double range, double sill,
  double nugget, double max_lag, double power)
{
  int i, j, mid;
  extern int nsearch;
  extern float  *sv_to_cell, **sv_btw_smpl;
  extern double *smpl_dist, **dist_btw_smpl;
  mid = sill - nugget;

  switch (model_num)
  {
    /* Spherical Model */
  case 0:
    for (i = 0; i < nsearch; i++)
    {
      if (smpl_dist[i] > 0 && smpl_dist[i] < range)
      {
	sv_to_cell[i] = mid * ((1.5 * smpl_dist[i] / range) -
		       (0.5 * pow (smpl_dist[i] / range, 3.0))) + nugget;
      }
      else
	sv_to_cell[i] = mid + nugget;

      for (j = i + 1; j < nsearch; j++)
      {
	if (dist_btw_smpl[i][j] > 0 && dist_btw_smpl[i][j] <= range)
	  sv_btw_smpl[i][j] = mid * ((1.5 * dist_btw_smpl[i][j] / range) -
		(0.5 * pow (dist_btw_smpl[i][j] / range, 3.0))) + nugget;
	else if (dist_btw_smpl[i][j] > range)
	  sv_btw_smpl[i][j] = mid + nugget;
      }
    }
    break;

    /* Power Model */
  case 1:
    for (i = 0; i < nsearch; i++)
    {
      sv_to_cell[i] = (mid / pow (max_lag, power)) *
	pow (smpl_dist[i], power) + nugget;
      for (j = i + 1; j < nsearch; j++)
      {
	sv_btw_smpl[i][j] = (mid / pow (max_lag, power)) *
	  pow (dist_btw_smpl[i][j], power) + nugget;
      }
    }
    break;

    /* Gaussian model */
  case 2:
    for (i = 0; i < nsearch; i++)
    {
      sv_to_cell[i] = mid * (1 - exp (-3 * pow (smpl_dist[i] / range, 2.0)))
	+ nugget;
      for (j = i + 1; j < nsearch; j++)
      {
	sv_btw_smpl[i][j] = mid * (1 - exp (-3 *
		       pow (dist_btw_smpl[i][j] / range, 2.0))) + nugget;
      }
    }
    break;

    /* Exponential Model */
  case 3:
    for (i = 0; i < nsearch; i++)
    {
      sv_to_cell[i] = mid * (1 - exp (-smpl_dist[i] / range)) + nugget;
      for (j = i + 1; j < nsearch; j++)
      {
	sv_btw_smpl[i][j] = mid * (1 - exp (-dist_btw_smpl[i][j] / range))
	  + nugget;
      }
    }
    break;

    /* Logarithmic Model */
  case 4:
    for (i = 0; i < nsearch; i++)
    {
      sv_to_cell[i] = (mid / log10 (max_lag)) * log10 (smpl_dist[i]) + nugget;
      for (j = i + 1; j < nsearch; j++)
      {
	sv_btw_smpl[i][j] = (mid / log10 (max_lag)) *
	  log10 (dist_btw_smpl[i][j]) + nugget;
	/*
	 * 
	 * fprintf(stderr,"\nsv_btw_smpl[%d][%d]=%lf",i,j,sv_btw_smpl[i][j]
	 * );
	 */
      }
    }
    break;
  }
}
