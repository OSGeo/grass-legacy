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

/*  Rewind vector data file to cause reads to start at beginning */
/* returns 0 on success, -1 on error */

static int
rew_dummy ()
{
      return -1;
}
static int format () { G_fatal_error ("Requested format is not compiled in this version"); return 0; } 

static int (*Rewind_array[][3]) () =
{
    { rew_dummy, V1_rewind_nat, V2_rewind_nat }
#ifdef HAVE_OGR
   ,{ rew_dummy, V1_rewind_ogr, V2_rewind_ogr }
#else
   ,{ rew_dummy, format, format }
#endif
};

/*!
 \fn int Vect_rewind (struct Map_info *Map)
 \brief Rewind vector data file to cause reads to start at beginning
 \return 0 on success, -1 on error
 \param Map_info structure
*/
int 
Vect_rewind (struct Map_info *Map)
{
    if (!VECT_OPEN (Map))
        return -1;

    G_debug (1, "Vect_Rewind(): name = %s", Map->name);
    return (*Rewind_array[Map->format][Map->level]) (Map);
}

