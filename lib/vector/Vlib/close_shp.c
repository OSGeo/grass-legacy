/*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Original author CERL, probably Dave Gerdes or Mike Higgins.
*               Update to GRASS 5.7 Radim Blazek and David D. Gray.
*
* PURPOSE:      Higher level functions for reading/writing/manipulating vectors.
*
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include "Vect.h"
#include <stdlib.h>

/* 
** return 0 on success
**         non-zero on error
*/
int 
V1_close_shp (struct Map_info *Map)
{
  if (!VECT_OPEN (Map))
    return 1;

  /* TODO something extra for shp opened for writing ? */  
  if (Map->mode == GV_MODE_WRITE || Map->mode == GV_MODE_RW) {
    Vect__write_head (Map);
  }

  SHPClose( Map->fInfo.shp.hShp );
  DBFClose( Map->fInfo.shp.hDbf );

  return 0;
}


