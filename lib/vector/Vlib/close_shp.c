/*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Original author CERL, probably Dave Gerdes or Mike Higgins.
*               Update to GRASS 5.1 Radim Blazek and David D. Gray.
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
    return -1;

  /* TODO something extra for shp opened for writing ? */  
  if (Map->mode == GV_MODE_WRITE || Map->mode == GV_MODE_RW)
    Vect__write_head (Map);

  free (Map->name);
  free (Map->mapset);
  free (Map->digit_file);

  Map->name = NULL;
  Map->mapset = NULL;
  Map->digit_file = NULL;
  Map->open = VECT_CLOSED_CODE;

  SHPClose( Map->fInfo.shp.hShp );
  DBFClose( Map->fInfo.shp.hDbf );

  return 0;
}

/* 
** return 0 on success
**         non-zero on error
*/
int 
V2_close_shp (struct Map_info *Map)
{
    struct Coor_info CInfo;
    struct Plus_head *Plus;
	
    G_debug (1, "V2_close_shp()" );

    Plus = &(Map->plus);
    
    if (Plus->mode & (GV_MODE_WRITE | GV_MODE_RW)) { 
	Vect_coor_info ( Map, &CInfo);
	Plus->coor_size = CInfo.size;
	Plus->coor_mtime = CInfo.mtime;

	Vect_save_topo ( Map );
	Vect_save_spatial_index ( Map );
    } 
        
    dig_free_plus ( Plus );

    V1_close_shp (Map);
	
    return -1;
}


