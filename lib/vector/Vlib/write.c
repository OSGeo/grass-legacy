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

static long write_dummy () { 
    G_warning ( "Vect_write_line() for this format/level not supported");
    return -1; 
}
static int rewrite_dummy () { 
    G_warning ( "Vect_rewrite_line() for this format/level not supported");
    return -1; 
}
static int  delete_dummy () { 
    G_warning ( "Vect_delete_line() for this format/level not supported");
    return -1; 
}

static long (*Write_line_array[][3]) () =
{
    { write_dummy, V1_write_line_nat, V2_write_line_nat } 
   ,{ write_dummy, write_dummy, write_dummy }
#ifdef HAVE_POSTGRES
   ,{ write_dummy, V1_write_line_post, write_dummy } 
#endif
};

static int (*Vect_rewrite_line_array[][3]) () =
{
    { rewrite_dummy, rewrite_dummy, V2_rewrite_line_nat } 
   ,{ rewrite_dummy, rewrite_dummy, rewrite_dummy }
#ifdef HAVE_POSTGRES
   ,{ rewrite_dummy, rewrite_dummy, rewrite_dummy } 
#endif
};

/*
static int (*V1_delete_line_array[][3]) () =
{
    { delete_dummy, V1_delete_line_nat, delete_dummy } 
   ,{ delete_dummy, delete_dummy, delete_dummy }
#ifdef HAVE_POSTGRES
   ,{ delete_dummy, V1_delete_line_post, delete_dummy } 
#endif
};
*/

static int (*Vect_delete_line_array[][3]) () =
{
    { delete_dummy, delete_dummy, V2_delete_line_nat } 
   ,{ delete_dummy, delete_dummy, delete_dummy }
#ifdef HAVE_POSTGRES
   ,{ delete_dummy, delete_dummy, delete_dummy } 
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
*  Returns: number of new line
*           -1 on error 
*/
int
Vect_rewrite_line (Map, line, type, points, cats )
     struct Map_info *Map;
     int line;
     int type;
     struct line_pnts *points;
     struct line_cats *cats;
{
#ifdef GDEBUG
    G_debug (3, "Vect_rewrite_line(): name = %s", Map->name);
#endif
    return (*Vect_rewrite_line_array[Map->format][Map->level]) (Map, line, type, points, cats);
}

/*
*  Deletes line at the given offset. Map must be opened on level 2.
*  
*  Returns: 0 ok
*          -1 on error 
*/
/*
int
V1_delete_line (Map, offset )
     struct Map_info *Map;
     long offset;
{
#ifdef GDEBUG
    G_debug (3, "V1_delete_line(): name = %s", Map->name);
#endif
    return (*V1_delete_line_array[Map->format][Map->level]) (Map, offset);
}
*/

/*
*  Deletes line of given number. Map must be opened on level 2.
*  
*  Returns: 0 ok
*          -1 on error 
*/
int
Vect_delete_line (Map, line )
     struct Map_info *Map;
     int line;
{
    int ret;
    
    G_debug (3, "Vect_delete_line(): name = %s", Map->name);
    
    if ( Map->level < 2 ) {
	G_warning ("Cannot delete the line, map '%s' is not opened on level 2", Map->name );
        return -1;
    }
    
    if ( Map->mode != GV_MODE_RW && Map->mode != GV_MODE_WRITE ) {
	G_warning ("Cannot delete the line, map '%s' is not in opened in 'write' mode", Map->name );
        return -1;
    }
    
    ret = (*Vect_delete_line_array[Map->format][Map->level]) (Map, line);

    if ( ret == -1 )
	G_warning ( "Vect_delete_line() failed");
    
    return ret;
}

