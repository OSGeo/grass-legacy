/******************************************************************************
 * bounds.c
 * Get the extent of the import's region and store to
 * static buffer

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>`
 * 26th. Oct. 2000
 * Last updated 4th. Jan. 2001
 *

 * This file is part of GRASS GIS. It is free software. You can 
 * redistribute it and/or modify it under the terms of 
 * the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option)
 * any later version.
 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 ******************************************************************************/

#include <stdlib.h>
#include<stdio.h>
#include <string.h>
#include "gis.h"
#include "shapefil.h"
#include "bounds.h"
#include "dbutils.h"


int set_bounds(SHPHandle hs, region *bb) {


  /* Main structures */

  int *dummy1, *dummy2;
  double *minvals, *maxvals;
  double minx, maxx, miny, maxy;
  char renderbuf[128];

  long ilbx, ilby;
  int ilen, ilen0;
  double flbx, flby;
  
  dummy1 = (int *)G_malloc( sizeof(int) );
  dummy2 = (int *)G_malloc( sizeof(int) );
  minvals = (double *)G_malloc(4 * sizeof(double) );
  maxvals = (double *)G_malloc(4 * sizeof(double) );

  SHPGetInfo(hs, dummy1, dummy2, minvals, maxvals);

  bb->w = minx = minvals[0];
  bb->s = miny = minvals[1];
  bb->e = maxx = maxvals[0];
  bb->n = maxy = maxvals[1];

  G_free(minvals);
  G_free(maxvals);
  G_free(dummy1);
  G_free(dummy2);

  if( minx < 0.0 )
    ilbx = (long)minx - 1;
  else
    ilbx = (long)minx;

  flbx = - (double)ilbx - 2.0;  /* Make some space for matrix windows */


  if( miny < 0.0 )
    ilby = (long)miny - 1;
  else
    ilby = (long)miny;

  flby = - (double)ilby - 2.0; /* Make some space for matrix windows */

  sprintf(renderbuf, "%.0lf", maxx + flbx + 2.0);
  ilen = strlen(renderbuf);

  sprintf(renderbuf, "%.0lf", maxy + flby + 2.0);
  ilen0 = strlen(renderbuf);
  if(ilen0 > ilen) ilen = ilen0;

  if( proc_key_params( SET_VAL, &ilen, &flbx, &flby ) ) {
    fprintf(stderr, "Could not set parameters for setting key values. Aborting." );
    return -1;
  }

  return 0;
  
}
