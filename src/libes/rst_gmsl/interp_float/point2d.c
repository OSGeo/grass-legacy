/*-
 *
 * Original program and various modifications:
 * Lubos Mitas 
 *
 * GRASS4.1 version of the program and GRASS4.2 modifications:
 * H. Mitasova
 * I. Kosinovsky, D. Gerdes
 * D. McCauley 
 *
 * Copyright 1993, 1995:
 * L. Mitas ,
 * H. Mitasova ,
 * I. Kosinovsky, ,
 * D.Gerdes 
 * D. McCauley
 *
 * modified by McCauley in August 1995
 * modified by Mitasova in August 1995, Nov. 1996
 *
 */


#include <stdio.h>
#include <math.h>
#include <unistd.h>
#include "gis.h"
#include "site.h"

#include "interpf.h"

int IL_check_at_points_2d (
    struct interp_params *params,
    struct quaddata *data,	/* current region */
    double *b,			/* solution of linear equations */
    double *ertot,		/* total error */
    double zmin,			/* min z-value */
    double dnorm
)

/*
 * Checks if interpolating function interp() evaluates correct z-values at
 * given points. If smoothing is used calculate the maximum error caused
 * by smoothing.
 */

{
  int n_points = data->n_points;/* number of points */
  struct triple *points = data->points;	/* points for interpolation */
  double east = data->xmax;
  double west = data->x_orig;
  double north = data->ymax;
  double south = data->y_orig;
  double rfsta2, errmax, h, xx, yy, r2, hz, zz, err, xmm, ymm, zmm,
   r;
  int n1, mm, m, mmax;
  double fstar2;
  char desc[100];
  int inside;
  Site *site;

  if ((site = G_site_new_struct (-1, 2, 0, 1)) == NULL)
    G_fatal_error ("Memory error for site struct");

  fstar2 = params->fi * params->fi / 4.;
  errmax = .0;
  n1 = n_points + 1;
  for (mm = 1; mm <= n_points; mm++)
  {
    h = b[0];
    for (m = 1; m <= n_points; m++)
    {
      xx = points[mm - 1].x - points[m - 1].x;
      yy = points[mm - 1].y - points[m - 1].y;
      r2 = yy * yy + xx * xx;
      if (r2 != 0.)
      {
	rfsta2 = fstar2 * r2;
	r = r2;
	h = h + b[m] * params->interp (r, params->fi);
      }
    }
/* modified by helena january 1997 - normalization of z was
   removed from segm2d.c and interp2d.c
    hz = (h * dnorm) + zmin;
    zz = (points[mm - 1].z * dnorm) + zmin;
*/
    hz = h + zmin;
    zz = points[mm - 1].z + zmin;
    err = hz - zz;
    xmm = points[mm - 1].x * dnorm + params->x_orig + west;
    ymm = points[mm - 1].y * dnorm + params->y_orig + south;
    if ((xmm >= west + params->x_orig) && (xmm <= east + params->x_orig) &&
      (ymm >= south + params->y_orig) && (ymm <= north + params->y_orig))
      inside = 1;
    else
      inside = 0;

    if (params->fddevi != NULL)
    {
      site->dbl_att[0] = err;
      site->east = xmm;
      site->north = ymm;
      if (inside)		/* if the point is inside the region */
	G_site_put (params->fddevi, site);
    }
    if (inside)
      (*ertot) += err * err;

    /*
     * if (err >= errmax) { errmax = err; mmax = mm; }
     */
  }
  /*
   * ertot = amax1 (errmax, *ertot);
   */
  return 1;
}
