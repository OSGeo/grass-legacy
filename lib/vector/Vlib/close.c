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
#include <stdlib.h>
#include <stdio.h>
#include "Vect.h"

static int
clo_dummy () {
    return -1;
}

static int format () { G_fatal_error ("Requested format is not compiled in this version"); return 0; }

static int (*Close_array[][3]) () =
{
     { clo_dummy, V1_close_nat, V2_close_nat }
   , { clo_dummy, V1_close_shp, V2_close_shp } 
#ifdef HAVE_POSTGRES
   , { clo_dummy, V1_close_post, V2_close_post }
#else
   ,{ clo_dummy, format, format }
#endif
#ifdef HAVE_OGR
   , { clo_dummy, V1_close_ogr, V2_close_ogr }
#else
   ,{ clo_dummy, format, format }
#endif
};


/*!
 \fn int Vect_close (struct Map_info *Map)
 \brief close vector data file
 \return 0 on success, non-zero on error
 \param Map_info structure
*/
int 
Vect_close (struct Map_info *Map)
{
    
    G_debug (1, "Vect_close(): name = %s, mapset = %s, format = %d, level = %d",
	         Map->name, Map->mapset, Map->format, Map->level);

    if ( Map->format == GV_FORMAT_NATIVE || Map->format == GV_FORMAT_POSTGIS ) {
	if ( Map->hist_fp != NULL ) fclose ( Map->hist_fp );
    }

    return (*Close_array[Map->format][Map->level]) (Map); 

}

