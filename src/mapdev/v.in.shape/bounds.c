/******************************************************************************
 * bounds.c
 * Get the extent of the import's region and store to
 * static buffer

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>`
 * 26th. Oct. 2000
 * Last updated 26th. Oct. 2000
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


int set_bounds(SHPHandle hs) {


  /* Main structures */

  double *minvals, *maxvals;
  double minx, maxx, miny, maxy;
  char renderbuf[128];

  int ilbx, ilby, ilen, ilen0;
  double flbx, flby;

  minvals = (double *)G_malloc(4 * sizeof(double) );
  maxvals = (double *)G_malloc(4 * sizeof(double) );

  SHPGetInfo(hs, NULL, NULL, minvals, maxvals);

  minx = minvals[0];
  miny = minvals[1];
  maxx = maxvals[0];
  maxy = maxvals[1];

  G_free(minvals);
  G_free(maxvals);

  if( minx < 0.0 )
    ilbx = (int)minx - 1;
  else
    ilbx = (int)minx;

  flbx = - (double)ilbx;


  if( miny < 0.0 )
    ilby = (int)miny - 1;
  else
    ilby = (int)miny;

  flby = - (double)ilby;

  sprintf(renderbuf, "%.0lf", flbx);
  ilen = strlen(renderbuf);

  sprintf(renderbuf, "%.0lf", flby);
  ilen0 = strlen(renderbuf);
  if(ilen0 > ilen) ilen = ilen0;

  if( proc_key_params( SET_VAL, &ilen, &flbx, &flby ) ) {
    fprintf(stderr, "Could not set parameters for setting key values. Aborting." );
    return -1;
  }

  return 0;
  
}
