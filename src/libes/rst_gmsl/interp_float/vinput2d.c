/*-
 * Written by H. Mitasova, I. Kosinovsky, D. Gerdes Fall 1993
 * University of Illinois
 * US Army Construction Engineering Research Lab  
 * Copyright 1993, H. Mitasova (University of Illinois),
 * I. Kosinovsky, (USA-CERL), and D.Gerdes (USA-CERL)   
 *
 * modified by McCauley in August 1995
 * modified by Mitasova in August 1995  
 * modofied by Mitasova in Nov 1999 (dmax fix)
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "Vect.h"
#include "gis.h"
#include "bitmap.h"
#include "linkm.h"

#include "interpf.h"

int IL_vector_input_data_2d (
    struct interp_params *params,
    struct Map_info *Map,		/* input vector file */
    struct Categories *cats,	/* Cats file */
    int iselev,			/* do zeroes reprezent elevation? */
    struct tree_info *info,	/* quadtree info */
    double *xmin,
    double *xmax,
    double *ymin,
    double *ymax,
    double *zmin,
    double *zmax,
    int *n_points,		/* number of points used for interpolation */
    double *dmax
)

/*
 * Inserts input data inside the region into a quad tree. Also translates
 * data. Returns number of segments in the quad tree.
 */

{
  double  dmax2;	/* max distance between points squared*/
  struct triple *point;
  double east, north, value, x, y, z;
  char *desc;
  double c1, c2, c3, c4;
  int k = 0;
  double deltx, delty;
  int a;
  char *buf[1024];
  int i, j;
  int first_time = 1;
  double ns_res, ew_res;
  int npoint, OUTRANGE, NPT;
  int totsegm;
  struct quaddata *data = (struct quaddata *) info->root->data;
  struct TimeStamp ts;
  int type;
  register int i1, tmp;
  double *xptr, *yptr, xprev, yprev, x1, y1, d1, xt, yt;
  static struct line_pnts *Points;
  char buf1[100], buf2[100];
  char *p;
  int att, n_points1, times, j1, k1, ind1, ind2;
  int prev = 0;
  int isnode = 0;
  OUTRANGE = 0;
  npoint = 0;


  ns_res = (data->ymax - data->y_orig) / data->n_rows;
  ew_res = (data->xmax - data->x_orig) / data->n_cols;
  dmax2=*dmax * *dmax;

  Points = Vect_new_line_struct ();	/* init line_pnts struct */
  Vect_set_constraint_type (Map, LINE | DOT);

  for (i1 = 1; i1 <= Map->n_lines; i1++)
  {
    if (Map->Line[i1].att > 0)
    {
      if (0 > V2_read_line (Map, Points, i1))
      {
	fprintf (stderr, "Read error\n");
	return -1;
      }

      xptr = Points->x;
      yptr = Points->y;
      prev = 0;
      n_points1 = Points->n_points;

      while (n_points1--)
      {
	if ((n_points1 == Points->n_points - 1) || (n_points1 == 0))
	  isnode = 1;
	else
	  isnode = 0;
	att = Map->Att[Map->Line[i1].att].cat;
	if (cats)
	{
	  p = G_get_cat (att, cats);
	  sscanf (p, "%d", &att);
	}

	if (prev == 0)
	{
	  xprev = *xptr;
	  yprev = *yptr;
	  prev = 1;
	  if (!isnode)
	  {
	    process_point (*xptr++, *yptr++, (double) att, info, params->zmult, xmin,
	    xmax, ymin, ymax, zmin, zmax, &npoint, &OUTRANGE, iselev, &k);
	  }
	}
	else
	{
	  /* compare the distance between current and previous */
	  x1 = *xptr;
	  y1 = *yptr;
	  xt = fabs (x1 - xprev);
	  y1 = *yptr;
	  xt = fabs (x1 - xprev);
	  yt = fabs (y1 - yprev);
	  d1 = (xt * xt + yt * yt);
	  if ((d1 > dmax2) && (dmax2 != 0.))
	  {
	    times = (int) (d1 / dmax2 + 0.5);
	    for (j1 = 0; j1 < times; j1++)
	    {
	      xt = x1 - j1 * ((x1 - xprev) / times);
	      yt = y1 - j1 * ((y1 - yprev) / times);
	      if ((!isnode) || (j1 != 0))
	      {
		process_point (xt, yt, (double) att, info, params->zmult, xmin,
			       xmax, ymin, ymax, zmin, zmax, &npoint, &OUTRANGE, iselev, &k);
	      }
	      isnode = 0;

	    }
	    xptr++;
	    yptr++;
	  }
	  else
	  {
	    if (!isnode)
	    {
	      process_point (*xptr++, *yptr++, (double) att, info, params->zmult, xmin,
			     xmax, ymin, ymax, zmin, zmax, &npoint, &OUTRANGE, iselev, &k);
	    }
	  }
	  xprev = x1;
	  yprev = y1;
	}

      }
    }
  }
  for (k1 = 1; k1 <= Map->n_nodes; k1++)
  {
    x1 = Map->Node[k1].x;
    y1 = Map->Node[k1].y;
    /* att = Map->Att[Map->Line[i1].att].cat;  */
    if (Map->Node[k1].lines != NULL)
    {
      ind1 = abs (Map->Node[k1].lines[0]);
      ind2 = Map->Line[ind1].att;
      att = Map->Att[ind2].cat;
      if (cats)
      {
	p = G_get_cat (att, cats);
	sscanf (p, "%d", &att);
      }
      process_point (x1, y1, (double) att, info, params->zmult,
      xmin, xmax, ymin, ymax, zmin, zmax, &npoint, &OUTRANGE, iselev, &k);
    }
  }
  c1 = *xmin - data->x_orig;
  c2 = data->xmax - *xmax;
  c3 = *ymin - data->y_orig;
  c4 = data->ymax - *ymax;
  if ((c1 > 5 * ew_res) || (c2 > 5 * ew_res) || (c3 > 5 * ns_res) || (c4 > 5 * ns_res))
  {
    static int once = 0;

    if (!once)
    {
      once = 1;
      fprintf (stderr, "Warning: strip exists with insufficient data\n");
    }
  }

  totsegm = translate_quad (info->root, data->x_orig, data->y_orig, *zmin, 4);
  if (!totsegm)
    return 0;
  data->x_orig = 0;
  data->y_orig = 0;

  /* G_read_vector_timestamp(name,mapset,ts); */

  fprintf (stderr, "\n");
  if (OUTRANGE > 0)
    fprintf (stderr, "Warning: there are points outside specified region--ignored %d points\n", OUTRANGE);
  if (npoint > 0)
    fprintf (stderr, "Warning: ignoring %d points -- too dense\n", npoint);
  npoint = k - npoint - OUTRANGE;
  if (npoint < params->kmin)
  {
    if (npoint != 0)
    {
      fprintf (stderr, "WARNING: %d points given for interpolation (after thinning) is less than given NPMIN=%d\n", npoint, params->kmin);
      params->kmin = npoint;
    }
    else
    {
      fprintf (stderr, "ERROR2: zero points in the given region!\n");
      return -1;
    }
  }
  if (npoint > params->KMAX2 && params->kmin <= params->kmax)
  {
    fprintf (stderr, "ERROR: segmentation parameters set to invalid values: npmin= %d, segmax= %d \n", params->kmin, params->kmax);
    fprintf (stderr, "for smooth connection of segments, npmin > segmax (see manual) \n");
    return -1;
  }
  if (npoint < params->KMAX2 && params->kmax != params->KMAX2)
    fprintf (stderr, "Warning : there is less than %d points for interpolation, no segmentation is necessary, to run the program faster, set segmax=%d (see manual)\n", params->KMAX2, params->KMAX2);

  fprintf (stderr, "\n");
  fprintf (stderr, "The number of points from vector file is %d\n", k);
  fprintf (stderr, "The number of points outside of region %d\n", OUTRANGE);
  fprintf (stderr, "The number of points being used is %d\n", npoint);
  *n_points = npoint;
  return (totsegm);
}

int process_point (
    double x,
    double y,
    double z,
    struct tree_info *info,	/* quadtree info */
    double zmult,			/* multiplier for z-values */
    double *xmin,
    double *xmax,
    double *ymin,
    double *ymax,
    double *zmin,
    double *zmax,
    int *npoint,
    int *OUTRANGE,
    int iselev,
    int *total
)

{
  struct triple *point;
  double east, north, value;
  double c1, c2, c3, c4;
  int k = 0;
  double deltx, delty;
  int a;
  char *buf[1024];
  int i, j;
  static int first_time = 1;
  double ns_res, ew_res;
  struct quaddata *data = (struct quaddata *) info->root->data;


  (*total)++;


  z = z * zmult;
  c1 = x - data->x_orig;
  c2 = data->xmax - x;
  c3 = y - data->y_orig;
  c4 = data->ymax - y;

  if (!((c1 >= 0) && (c2 >= 0) && (c3 >= 0) && (c4 >= 0)))
  {
    if (!(*OUTRANGE))
    {
      fprintf (stderr, "Warning: some points outside of region -- will ignore...\n");
    }
    (*OUTRANGE)++;
  }
  else
  {
    if (!((z == 0.) && (!iselev)))
    {
      if (!(point = quad_point_new (x, y, z, 0.)))
      {
	fprintf (stderr, "cannot allocate memory for point\n");
	return -1;
      }
      a = MT_insert (point, info, info->root, 4);
      if (a == 0)
      {
	(*npoint)++;
      }
      if (a < 0)
      {
	fprintf (stderr, "cannot insert %f,%f,%f a = %d\n", x, y, z, a);
	return -1;
      }
      free (point);
      if (first_time)
      {
	first_time = 0;
	*xmin = x;
	*ymin = y;
	*zmin = z;
	*xmax = x;
	*ymax = y;
	*zmax = z;
      }
      *xmin = amin1 (*xmin, x);
      *ymin = amin1 (*ymin, y);
      *zmin = amin1 (*zmin, z);
      *xmax = amax1 (*xmax, x);
      *ymax = amax1 (*ymax, y);
      *zmax = amax1 (*zmax, z);
    }
  }
  return 1;
}
