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

static int format () { G_fatal_error ("Requested format is not compiled in this version"); return 0; }

static long (*Write_line_array[][3]) () =
{
    { write_dummy, V1_write_line_nat, V2_write_line_nat } 
#ifdef HAVE_OGR
   ,{ write_dummy, write_dummy, write_dummy }
#else
   ,{ write_dummy, format, format }
#endif
};

static int (*Vect_rewrite_line_array[][3]) () =
{
    { rewrite_dummy, rewrite_dummy, V2_rewrite_line_nat } 
#ifdef HAVE_OGR
   ,{ rewrite_dummy, rewrite_dummy, rewrite_dummy }
#else
   ,{ rewrite_dummy, format, format }
#endif
};

static int (*Vect_delete_line_array[][3]) () =
{
    { delete_dummy, delete_dummy, V2_delete_line_nat } 
#ifdef HAVE_OGR
   ,{ delete_dummy, delete_dummy, delete_dummy } 
#else
   ,{ delete_dummy, format, format }
#endif
};

/*!
 \fn long Vect_write_line (
     struct Map_info *Map,
     int type,
     struct line_pnts *points,
     struct line_cats *cats)
 \brief writes new line to the end of file (table)
 \return offset into file where the line starts, -1 on error
 \param Map_info structure, vector type, line_pnts structure, line_cats structure
*/
long
Vect_write_line (
     struct Map_info *Map,
     int type,
     struct line_pnts *points,
     struct line_cats *cats)
{
    G_debug (3, "Vect_write_line(): name = %s, format = %d, level = %d", 
	           Map->name, Map->format, Map->level);

    if (!VECT_OPEN (Map))
	return -1; 

    dig_line_reset_updated ( &(Map->plus) );
    dig_node_reset_updated ( &(Map->plus) );
    if ( !(Map->plus.update_cidx) ) {
        Map->plus.cidx_up_to_date = 0;
    }

    return (*Write_line_array[Map->format][Map->level]) (Map, type, points, cats);
}


/*!
 \fn int Vect_rewrite_line (
     struct Map_info *Map,
     int line,
     int type,
     struct line_pnts *points,
     struct line_cats *cats)
 \brief rewrites line info at the given offset. The number of points
   or cats or type may change. If necessary, the old line is deleted and
   new is written.
 \return number of new line, -1 on error
 \param Map_info structure, line number, vector type, line_pnts structure, line_cats structure
*/
int
Vect_rewrite_line (
     struct Map_info *Map,
     int line,
     int type,
     struct line_pnts *points,
     struct line_cats *cats)
{
    G_debug (3, "Vect_rewrite_line(): name = %s", Map->name);
    
    dig_line_reset_updated ( &(Map->plus) );
    dig_node_reset_updated ( &(Map->plus) );
    if ( !(Map->plus.update_cidx) ) {
        Map->plus.cidx_up_to_date = 0;
    }

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
V1_delete_line (
     struct Map_info *Map,
     long offset)
{
#ifdef GDEBUG
    G_debug (3, "V1_delete_line(): name = %s", Map->name);
#endif
    return (*V1_delete_line_array[Map->format][Map->level]) (Map, offset);
}
*/

/*!
 \fn int Vect_delete_line (
     struct Map_info *Map,
     int line)
 \brief deletes line of given number. Map must be opened on level 2.
 \return 0 on success, -1 on error
 \param Map_info structure, line number
*/
int
Vect_delete_line (
     struct Map_info *Map,
     int line)
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
    
    dig_line_reset_updated ( &(Map->plus) );
    dig_node_reset_updated ( &(Map->plus) );
    if ( !(Map->plus.update_cidx) ) {
        Map->plus.cidx_up_to_date = 0;
    }
    
    ret = (*Vect_delete_line_array[Map->format][Map->level]) (Map, line);

    if ( ret == -1 )
	G_warning ( "Vect_delete_line() failed");
    
    return ret;
}

