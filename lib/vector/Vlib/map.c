/*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Radim Blazek
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

/* Copy all alive elements of opened vector map to another opened vector map.
*  returns 0 on success
*         1 on error
*/
int 
Vect_copy_map_lines ( struct Map_info *In, struct Map_info *Out )
{
    int    type;
    struct line_pnts *Points;
    struct line_cats *Cats;

    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    
    while (1) {
	type =  Vect_read_next_line (In, Points, Cats);
	switch ( type ) {
            case -1:
		G_warning ("Cannot read vector file\n" );
                return 1;
            case -2: /* EOF */
 	        Vect_destroy_line_struct (Points);
	        Vect_destroy_cats_struct (Cats);
                return  0;
	    case  0: /* dead line */
		continue;
	}
       	Vect_write_line ( Out, type, Points, Cats );
    }

    
    /* Not reached */
}

