
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
#include <stdlib.h>
#include <stdio.h>
#include "krig.h"

void variogram_model (int model_num, double range, double sill,
  double nugget, double max_lag, double power)
{
  int i, j;
  double mid;
  double d1;
  extern int nsearch;
  extern mat_struct  *sv_to_cell, *sv_btw_smpl;
  extern mat_struct *smpl_dist, *dist_btw_smpl;
  mid = sill - nugget;

  switch (model_num)
  {
    /* Spherical Model */
  case 0:
    {
      for (i = 0; i < nsearch; i++)
	{
	  d1 = G_matrix_get_element(smpl_dist, i, 0);
	  if(d1 > 0.0 && d1 < range)
	    {
	      d1 /= range;
	      G_matrix_set_element(sv_to_cell, i, 0, mid * ((1.5 * d1) -
							    (0.5 * pow(d1, 3.0)))
				   + nugget);
	    }
	  else
	    G_matrix_set_element(sv_to_cell, i, 0, mid + nugget);

	  for (j = i + 1; j < nsearch; j++)
	    {
	      d1 = G_matrix_get_element(dist_btw_smpl, i, j);
	      if (d1 > 0 && d1 <= range) {
		d1 /= range;
		G_matrix_set_element(sv_btw_smpl, i, j, mid * ((1.5 * d1) -
							       (0.5 * pow (d1, 3.0)))
				     + nugget);
	      }
	      else if (d1 > 1.0)
		G_matrix_set_element(sv_btw_smpl, i, j, mid + nugget);
	      else { /* SHOULDN'T GET HERE ? */
		fprintf(stderr, "Error: Code shouldn't be reached.\n");
		exit(-1);
	      }
	    }
	}
    break;
    }

    /* Power Model */
  case 1:
    {
      for (i = 0; i < nsearch; i++)
	{
	  G_matrix_set_element(sv_to_cell, i, 0, (mid / pow (max_lag, power)) *
			       pow (G_matrix_get_element(smpl_dist, i, 0), power) + nugget);
	  for (j = i + 1; j < nsearch; j++)
	    {
	      G_matrix_set_element(sv_btw_smpl, i, j, (mid / pow (max_lag, power)) *
				   pow (G_matrix_get_element(dist_btw_smpl, i, j), power) + nugget);
	    }
	}
      break;
    }

    /* Gaussian model */
  case 2:
    {
      for (i = 0; i < nsearch; i++)
	{
	  G_matrix_set_element(sv_to_cell, i, 0, 
			       mid * (1 - exp (-3 * pow (G_matrix_get_element(smpl_dist, i, 0) / range, 2.0)))
			       + nugget);
	  for (j = i + 1; j < nsearch; j++)
	    {
	      G_matrix_set_element(sv_btw_smpl, i, j,  mid * 
				   (1 - exp (-3 *  pow (G_matrix_get_element(dist_btw_smpl, i, j) 
							/ range, 2.0))) + nugget);
	    }
	}
      break;
    }

    /* Exponential Model */
  case 3:
    {
      for (i = 0; i < nsearch; i++)
	{
	  G_matrix_set_element(sv_to_cell, i, 0, mid * 
			       (1 - exp (-G_matrix_get_element(smpl_dist, i, 0) / range)) 
			       + nugget);
	  for (j = i + 1; j < nsearch; j++)
	    {
	      G_matrix_set_element(sv_btw_smpl, i, j, mid * 
				   (1 - exp (-G_matrix_get_element(dist_btw_smpl, i, j) / range))
				   + nugget);
				   }
	    }
	  break;
	}

    /* Logarithmic Model */
  case 4:
    {
      for (i = 0; i < nsearch; i++)
	{
	  G_matrix_set_element(sv_to_cell, i, 0, (mid / log10 (max_lag)) * 
			       log10 (G_matrix_get_element(smpl_dist, i, 0)) + nugget);
	  for (j = i + 1; j < nsearch; j++)
	    {
	      G_matrix_set_element(sv_btw_smpl, i, j, (mid / log10 (max_lag)) *
				   log10 (G_matrix_get_element(dist_btw_smpl, i, j)) + nugget);
	    }
	}
      break;
    }
  }
}
