/*-
 * Written by H. Mitasova, I. Kosinovsky, D. Gerdes Fall 1993
 * University of Illinois
 * US Army Construction Engineering Research Lab  
 * Copyright 1993, H. Mitasova (University of Illinois),
 * I. Kosinovsky, (USA-CERL), and D.Gerdes (USA-CERL)   
 *
 * modified by McCauley in August 1995
 * modified by Mitasova in August 1995  
 * modified by Mitasova in November 1996 to include variable smoothing
 * modified by Brown in June 1999 - added elatt & smatt
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "gis.h"
#include "site.h"
#include "bitmap.h"
#include "linkm.h"

#include "interpf.h"

int IL_input_data_2d (
  struct interp_params *params,
  struct tree_info *info,	/* quadtree info */
  double *xmin,double *xmax,double *ymin,double *ymax,
  double *zmin, double *zmax,
  int *n_points)		/* number of points used for interpolation */

/*
 * Inserts input data inside the region into a quad tree. Also translates
 * data. Returns number of segments in the quad tree.
 */

{
  struct triple *point;
  double east, north, value, x, y, z, sm;   /* sm added Mit. 96)*/
  char *desc;
  double c1, c2, c3, c4;
  int k = 0;
  double deltx, delty;
  int a;
  char *buf[1024];
  int i, j;
  int first_time = 1;
  double ns_res, ew_res;
  int npoint, OUTRANGE;
  int totsegm;
  struct quaddata *data = (struct quaddata *) info->root->data;
  Site *site;
  Site_head head;

  OUTRANGE = 0;
  npoint = 0;
  sm = 0.;

  ns_res = (data->ymax - data->y_orig) / data->n_rows;
  ew_res = (data->xmax - data->x_orig) / data->n_cols;


  /* save the timestamp to put in history files */
  G_site_get_head (params->fdinp, &head);
  params->ts=head.time;

  {
      int dims, cat, strs, dbls;
  if (G_site_describe (params->fdinp, &dims, &cat, &strs, &dbls) != 0)
      G_fatal_error ("failed to guess site format");

  if ((site = G_site_new_struct (cat, dims, strs, dbls)) == NULL) 
      G_fatal_error ("Memory error for site struct");
  }

  /* selectable site attributes for elev & smoothing added June 99 -bb */

  while (G_site_get (params->fdinp, site) >= 0)
  {
    k++;
    x = site->east;
    y = site->north;
    z = site->dbl_att[params->elatt - 1] * params->zmult;
    if (params->smatt) sm=site->dbl_att[params->smatt - 1] * params->zmult; 
    c1 = x - data->x_orig;
    c2 = data->xmax - x;
    c3 = y - data->y_orig;
    c4 = data->ymax - y;

    if (!((c1 >= 0) && (c2 >= 0) && (c3 >= 0) && (c4 >= 0)))
    {
      if (!OUTRANGE)
      {
	fprintf (stderr, "Warning: some points outside of region -- will ignore...\n");
      }
      OUTRANGE++;
    }
    else
    {
      if (!(point = quad_point_new (x, y, z, sm))) /*sm added by Mit 96*/
      {
	fprintf (stderr, "cannot allocate memory for point\n");
      }
      a = MT_insert (point, info, info->root, 4);
      if (a == 0)
      {
	npoint++;
      }
      free (point);
      if (a < 0)
      {
	fprintf (stderr, "cannot insert %f,%f,%f a = %d\n", x, y, z, a);
	return -1;
      }

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
  /* fprintf (stderr, "\n"); */
  if (OUTRANGE > 0)
    fprintf (stderr, "Warning: there are points outside specified region--ignored %d points\n", OUTRANGE);
  if (npoint > 0)
    fprintf (stderr, "Warning: points are more dense than specified 'DMIN'--ignored %d points\n", npoint);
  npoint = k - npoint - OUTRANGE;
  if (npoint < params->kmin)
  {
    if (npoint != 0)
    {
      fprintf (stderr, "Warning: %d points given for interpolation (after thinning) is less than given NPMIN=%d\n", npoint, params->kmin);
      params->kmin = npoint;
    }
    else
    {
      fprintf (stderr, "ERROR1: zero points in the given region!\n");
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
    fprintf (stderr, "Warning: there is less than %d points for interpolation, no segmentation is necessary, to run the program faster, set segmax=%d (see manual)\n", params->KMAX2, params->KMAX2);

  fclose (params->fdinp);

  fprintf (stderr, "\n");
  fprintf (stderr, "The number of points in sites file is %d\n", k);
  fprintf (stderr, "The number of points outside of region %d\n", OUTRANGE);
  fprintf (stderr, "The number of points being used (after reduction) is %d\n", npoint);
  *n_points = npoint;
  return (totsegm);
}


int 
IL_create_bitmask (struct interp_params *params, struct BM *bitmask)

/** Creates a bitmap mask from given raster file **/
{
  int i, j, cfmask, irev;
  char *mapsetm;
  CELL *cellmask;
  char buf[1024];

  if (params->maskmap != NULL)
  {
    mapsetm = G_find_cell2 (params->maskmap, "");
    if (!mapsetm)
    {
      fprintf (stderr, "mask raster file [%s] not found\n", params->maskmap);
      return -1;
    }
    /*
     * bitmask = BM_create (params->nsizc, params->nsizr);
     */
    cellmask = G_allocate_cell_buf ();
    cfmask = G_open_cell_old (params->maskmap, mapsetm);
    for (i = 0; i < params->nsizr; i++)
    {
      irev = params->nsizr - i - 1;
      G_get_map_row (cfmask, cellmask, i);
      for (j = 0; j < params->nsizc; j++)
      {
	if (cellmask[j] == 0)
	  BM_set (bitmask, j, irev, 0);
	else
	  BM_set (bitmask, j, irev, 1);
      }
    }
    fprintf (stderr, "bitmap mask created\n");
  }
  else
    bitmask = NULL;
  return 1;
}

int translate_quad (
  struct multtree *tree,
  double numberx,
  double numbery,
  double numberz,
  int n_leafs)
{
  int total = 0, i, ii;
  struct triple *point;

  if (tree == NULL)
    return 0;
  if (tree->data == NULL)
    return 0;
  if (tree->leafs != NULL)
  {
    ((struct quaddata *) (tree->data))->x_orig -= numberx;
    ((struct quaddata *) (tree->data))->y_orig -= numbery;
    ((struct quaddata *) (tree->data))->xmax -= numberx;
    ((struct quaddata *) (tree->data))->ymax -= numbery;
    for (ii = 0; ii < n_leafs; ii++)
      total += translate_quad (tree->leafs[ii], numberx, numbery, numberz, n_leafs);
  }
  else
  {
    ((struct quaddata *) (tree->data))->x_orig -= numberx;
    ((struct quaddata *) (tree->data))->y_orig -= numbery;
    ((struct quaddata *) (tree->data))->xmax -= numberx;
    ((struct quaddata *) (tree->data))->ymax -= numbery;
    for (i = 0; i < ((struct quaddata *) (tree->data))->n_points; i++)
    {
      ((struct quaddata *) (tree->data))->points[i].x -= numberx;
      ((struct quaddata *) (tree->data))->points[i].y -= numbery;
      ((struct quaddata *) (tree->data))->points[i].z -= numberz;
    }
    return 1;
  }
  return total;
}
