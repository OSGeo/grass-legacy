/*
* $Id$
*
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

static long write_dummy () { return -1; }
static long rewrite_dummy () { return -1; }

static long (*Write_line_array[][2]) () =
{
    { write_dummy, V1_write_line_nat } 
   ,{ write_dummy, write_dummy }
#ifdef HAVE_POSTGRES
   ,{ write_dummy, V1_write_line_post } 
#endif
};

static long (*Rewrite_line_array[][2]) () =
{
    { rewrite_dummy, V1_rewrite_line_nat } 
   ,{ rewrite_dummy, rewrite_dummy }
#ifdef HAVE_POSTGRES
   ,{ rewrite_dummy, V1_rewrite_line_post } 
#endif
};

/* Writes new line to the end of file (table)
*
*  Returns: offset into file where the line starts
*           -1 on error 
*/
long
Vect_write_line (Map, type, points, cats)
     struct Map_info *Map;
     int type;
     struct line_pnts *points;
     struct line_cats *cats;
{
#ifdef GDEBUG
    G_debug (3, "Vect_write_line(): name = %s, format = %d, level = %d", 
	           Map->name, Map->format, Map->level);
#endif
    if (!VECT_OPEN (Map))
	return -1; 

    return (*Write_line_array[Map->format][Map->level]) (Map, type, points, cats);
}

/*
*  Rewrites line info at the given offset
*  the number of points or cats or type may change.
*  If necessary, the old line is deleted and new is written.
*  
*  Returns: offset into file where the line starts
*           -1 on error 
*/
long
Vect_rewrite_line (Map, offset, type, points, cats )
     struct Map_info *Map;
     long offset;
     int type;
     struct line_pnts *points;
     struct line_cats *cats;
{
#ifdef GDEBUG
    G_debug (3, "Vect_rewrite_line(): name = %s", Map->name);
#endif
    return (*Rewrite_line_array[Map->format][Map->level]) (Map, offset, type, points, cats);
}

