/*-
 * Written by H. Mitasova, L. Mitas, I. Kosinovsky, D. Gerdes Fall 1993
 * Copyright 1993, H. Mitasova 
 * I. Kosinovsky, and D.Gerdes   
 *
 * modified by McCauley in August 1995
 * modified by Mitasova in August 1995  
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "gis.h"

#include "interpf.h"

int IL_interp_segments_new_2d (
    struct interp_params *params,
    struct tree_info *info,	/* info for the quad tree */
    struct multtree *tree,	/* current leaf of the quad tree */
    struct BM *bitmask,		/* bitmask */
    double zmin,
    double zmax,		/* min and max input z-values */
    double *zminac,
    double *zmaxac,	/* min and max interp. z-values */
    double *gmin,
    double *gmax,			/* min and max inperp. slope val. */
    double *c1min,
    double *c1max,
    double *c2min,
    double *c2max,	/* min and max interp. curv. val. */
    double *ertot,		/* total interplating func. error */
    int totsegm,			/* total number of segments */
    int offset1,			/* offset for temp file writing */
    double dnorm
)

/*
 * Recursively processes each segment in a tree by a) finding points from
 * neighbouring segments so that the total number of points is between
 * KMIN and KMAX2 by calling tree function MT_get_region(). b) creating
 * and solving the system of linear equations using these points and
 * interp() by calling matrix_create() and G_ludcmp(). c) checking the
 * interpolating function values at points by calling check_points(). d)
 * computing grid for this segment using points and interp() by calling
 * grid_calc().
 */

{
  double xrmnp, xrmxp, yrmnp, yrmxp, xrmn, xrmx, yrmn, yrmx, xmn, xmx,
   edgex1, edgex2, temp1, distx, ymn, ymx, edgey1, edgey2, temp2, disty;	/* zzz; */
  int i, j, jj, npt, nptprev, MAXENC, NMIN, NMAX, rs, cs;
  int EDGE = 0, DEF = 0, DYNX, DYNY, points_total = 0, pt;

  struct quaddata *data, *datatemp;
  static int cursegm = 0;
  static double *b = NULL;
  static int *indx = NULL;
  static double **matrix = NULL;
  double ew_res, ns_res;

  ns_res = (((struct quaddata *) (info->root->data))->ymax -
       ((struct quaddata *) (info->root->data))->y_orig) / params->nsizr;
  ew_res = (((struct quaddata *) (info->root->data))->xmax -
       ((struct quaddata *) (info->root->data))->x_orig) / params->nsizc;

  if (tree == NULL)
    return -1;
  if (tree->data == NULL)
    return -1;
  if (((struct quaddata *) (tree->data))->points == NULL)
  {
    for (i = 0; i < 4; i++)
    {
      IL_interp_segments_new_2d (params, info, tree->leafs[i],
			 bitmask, zmin, zmax, zminac, zmaxac, gmin, gmax,
	     c1min, c1max, c2min, c2max, ertot, totsegm, offset1, dnorm);
    }
    return 1;
  }
  else
  {
    edgex1 = 0.;
    edgex2 = ((struct quaddata *) (info->root->data))->xmax;
    edgey1 = 0.;
    edgey2 = ((struct quaddata *) (info->root->data))->ymax;
    distx = (((struct quaddata *) (tree->data))->n_cols
	     * ew_res) * 0.2;
    disty = (((struct quaddata *) (tree->data))->n_rows
	     * ns_res) * 0.2;
    pt = ((struct quaddata *) (tree->data))->n_points;
    rs = ((struct quaddata *) (tree->data))->n_rows;
    cs = ((struct quaddata *) (tree->data))->n_cols;
    xmn = ((struct quaddata *) (tree->data))->x_orig;
    xmx = ((struct quaddata *) (tree->data))->xmax;
    ymn = ((struct quaddata *) (tree->data))->y_orig;
    ymx = ((struct quaddata *) (tree->data))->ymax;
    data = (struct quaddata *) quad_data_new (xmn, ymn, xmx,
					 ymx, rs, cs, pt, params->KMAX2);
    for (j = 0; j < pt; j++)
    {
      data->points[j] = ((struct quaddata *) (tree->data))->points[j];
      points_total++;
    }
    NMIN = (int) ((params->kmin - points_total) / 8.0 + 0.5);
    NMAX = (int) ((params->KMAX2 - points_total) / 8.0 - 0.5);
    for (j = 0; j < 8; j++)
    {
      npt = 0;
      switch (j)
      {
      case 0:
	xrmn = xmn - distx;
	xrmx = xmn;
	yrmn = ymn - disty;
	yrmx = ymn;
	DYNX = -1;
	DYNY = -1;
	break;
      case 1:
	xrmn = xmn;
	xrmx = xmx;
	yrmn = ymn - disty;
	yrmx = ymn;
	DYNX = 0;
	DYNY = -1;
	break;
      case 2:
	xrmn = xmx;
	xrmx = xmx + distx;
	yrmn = ymn - disty;
	yrmx = ymn;
	DYNX = 1;
	DYNY = -1;
	break;
      case 3:
	xrmn = xmx;
	xrmx = xmx + distx;
	yrmn = ymn;
	yrmx = ymx;
	DYNX = 1;
	DYNY = 0;
	break;
      case 4:
	xrmn = xmx;
	xrmx = xmx + distx;
	yrmn = ymx;
	yrmx = ymx + disty;
	DYNX = 1;
	DYNY = 1;
	break;
      case 5:
	xrmn = xmn;
	xrmx = xmx;
	yrmn = ymx;
	yrmx = ymx + disty;
	DYNX = 0;
	DYNY = 1;
	break;
      case 6:
	xrmn = xmn - distx;
	xrmx = xmn;
	yrmn = ymx;
	yrmx = ymx + disty;
	DYNX = -1;
	DYNY = 1;
	break;
      case 7:
	xrmn = xmn - distx;
	xrmx = xmn;
	yrmn = ymn;
	yrmx = ymx;
	DYNX = -1;
	DYNY = 0;
	break;
      default:
	break;
      }

      xrmnp = xrmx;
      xrmxp = xrmn;
      yrmnp = yrmx;
      yrmxp = yrmn;
      i = 0;
      MAXENC = 0;
      datatemp = (struct quaddata *) quad_data_new (xrmn, yrmn, xrmx,
					      yrmx, 0, 0, 0, NMAX + DEF);
      npt = MT_region_data (info, info->root, datatemp, NMAX + DEF, 4);

      while ((npt < NMIN + DEF) || (npt > NMAX + DEF))
      {
	i++;
	if (npt > NMAX + DEF)
	{
	  if (i >= 100)
	  {
	    fprintf (stderr, "Decreasing: Warning: taking too long to find points for interpolation--please change the region to area where your points are\n");
	    break;
	  }
	  MAXENC = 1;
	  nptprev = npt;
	  switch (DYNX)
	  {
	  case 0:
	    break;
	  case 1:
	    temp1 = xrmxp;
	    xrmxp = xrmx;
	    xrmx = xrmxp - fabs (xrmx - temp1) * 0.5;
	    break;
	  case -1:
	    temp1 = xrmnp;
	    xrmnp = xrmn;
	    xrmn = xrmnp + fabs (xrmn - temp1) * 0.5;
	  }
	  switch (DYNY)
	  {
	  case 0:
	    break;
	  case 1:
	    temp2 = yrmxp;
	    yrmxp = yrmx;
	    yrmx = yrmxp - fabs (yrmx - temp1) * 0.5;
	    break;
	  case -1:
	    temp2 = yrmnp;
	    yrmnp = yrmn;
	    yrmn = yrmnp + fabs (yrmn - temp2) * 0.5;
	  }
	}
	else
	{
	  if (i >= 100)
	  {
	    fprintf (stderr, "Increasing: taking too long to find points for interpolation--please change the region to area where your points are\n");
	    break;
	  }
	  nptprev = npt;
	  switch (DYNX)
	  {
	  case 0:
	    break;
	  case 1:
	    temp1 = xrmxp;
	    xrmxp = xrmx;
	    if (MAXENC)
	      xrmx = xrmxp + fabs (xrmx - temp1) * 0.5;
	    else
	      xrmx += (xrmx - xrmn);
	    break;
	  case -1:
	    temp1 = xrmnp;
	    xrmnp = xrmn;
	    if (MAXENC)
	      xrmn = xrmnp - fabs (xrmn - temp1) * 0.5;
	    else
	      xrmn -= (xrmx - xrmn);
	  }
	  switch (DYNY)
	  {
	  case 0:
	    break;
	  case 1:
	    temp2 = yrmxp;
	    yrmxp = yrmx;
	    if (MAXENC)
	      yrmx = yrmxp + fabs (yrmx - temp2) * 0.5;
	    else
	      yrmx += (yrmx - yrmn);
	    break;
	  case -1:
	    temp2 = yrmnp;
	    yrmnp = yrmn;
	    if (MAXENC)
	      yrmn = yrmnp - fabs (yrmn - temp2) * 0.5;
	    else
	      yrmn -= (yrmx - yrmn);
	    break;
	  }
	}
	if ((xrmn < edgex1) || (xrmx > edgex2) || (yrmn < edgey1) || (yrmx > edgey2))
	{
	  EDGE = 1;
	  break;
	}
	datatemp->x_orig = xrmn;/* update window */
	datatemp->y_orig = yrmn;
	datatemp->xmax = xrmx;
	datatemp->ymax = yrmx;
	datatemp->n_points = 0;
	nptprev = npt;
	npt = MT_region_data (info, info->root, datatemp, NMAX + DEF, 4);
      }				/* while loop */
      points_total += npt;
      if (DEF < 0)
	DEF = 0;

      for (jj = 0; jj < datatemp->n_points; jj++)
	data->points[data->n_points + jj] = datatemp->points[jj];

      data->n_points += datatemp->n_points;
      G_free (datatemp->points);
      G_free (datatemp);
    }				/* for loop */
    /* show before to catch 0% */
    if (totsegm != 0)
    {
      G_percent (cursegm, totsegm, 1);
    }

    data->n_rows = ((struct quaddata *) (tree->data))->n_rows;
    data->n_cols = ((struct quaddata *) (tree->data))->n_cols;

    /*
     * for printing out overlapping segments ((struct quaddata
     * *)(tree->data))->x_orig = xmn-distx; ((struct quaddata
     * *)(tree->data))->y_orig = ymn-disty; ((struct quaddata
     * *)(tree->data))->xmax = xmx+distx; ((struct quaddata
     * *)(tree->data))->ymax = ymx+disty;
     */
    data->x_orig = xmn;
    data->y_orig = ymn;
    data->xmax = xmx;
    data->ymax = ymx;

    if (!matrix)
    {
      if (!(matrix = G_alloc_matrix (params->KMAX2 + 1, params->KMAX2 + 1)))
      {
	fprintf (stderr, "Cannot allocate memory for matrix\n");
	return -1;
      }
    }
    if (!indx)
    {
      if (!(indx = G_alloc_ivector (params->KMAX2 + 1)))
      {
	fprintf (stderr, "Cannot allocate memory for indx\n");
	return -1;
      }
    }
    if (!b)
    {
      if (!(b = G_alloc_vector (params->KMAX2 + 3)))
      {
	fprintf (stderr, "Cannot allocate memory for b\n");
	return -1;
      }
    }
    for (i = 0; i < data->n_points; i++)
    {
      /*
       * 
       * fprintf(stderr,"point[%d]=%lf,%lf,%lf\n",i,data->points[i].x,data->
       * points[i].y,data->points[i].z);
       */
      data->points[i].x = (data->points[i].x - data->x_orig) / dnorm;
      data->points[i].y = (data->points[i].y - data->y_orig) / dnorm;
      data->points[i].z = data->points[i].z / dnorm;
    }

    if (params->matrix_create (params, data->points, data->n_points,
			       matrix, indx) < 0)
      return -1;

    for (i = 0; i < data->n_points; i++)
      b[i + 1] = data->points[i].z;
    b[0] = 0.;

    G_lubksb (matrix, data->n_points + 1, indx, b);

    params->check_points (params, data, b, ertot, zmin, dnorm);

    if ((params->Tmp_fd_z != NULL) || (params->Tmp_fd_dx != NULL) ||
	(params->Tmp_fd_dy != NULL) || (params->Tmp_fd_xx != NULL) ||
	(params->Tmp_fd_yy != NULL) || (params->Tmp_fd_xy != NULL))
    {

      if (params->grid_calc (params, data, bitmask,
			     zmin, zmax, zminac, zmaxac, gmin, gmax,
	       c1min, c1max, c2min, c2max, ertot, b, offset1, dnorm) < 0)
	return -1;
    }
    /* show after to catch 100% */
    cursegm++;
    if (totsegm < cursegm)
      fprintf (stderr, "%d %d\n", totsegm, cursegm);
    if (totsegm != 0)
    {
      G_percent (cursegm, totsegm, 1);
    }
    /*
     * G_free_matrix(matrix); G_free_ivector(indx); G_free_vector(b);
     */
    G_free (data->points);
    G_free (data);
    return 1;
  }
}
