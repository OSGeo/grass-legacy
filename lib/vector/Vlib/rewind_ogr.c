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

#ifdef HAVE_OGR

/* Rewind vector data file to cause reads to start at beginning. 
** returns 0 on success
**        -1 on error
*/
int 
V1_rewind_ogr (struct Map_info *Map)
{
    G_debug (2, "V1_rewind_ogr(): name = %s", Map->name);
    
    G_warning ("V1_rewind_ogr() not yet implemented" );
    return (-1);
    
    return 0;
}

int 
V2_rewind_ogr (struct Map_info *Map)
{
    G_debug (2, "V2_rewind_ogr(): name = %s", Map->name);

    Map->next_line = 1;
    
    return V1_rewind_ogr (Map);	
}

#endif 
