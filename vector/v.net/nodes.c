/***************************************************************
 *
 * MODULE:       v.net
 * 
 * AUTHOR(S):    Radim Blazek
 *               
 * PURPOSE:      Network maintenance
 *               
 * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 *
 *               This program is free software under the 
 *               GNU General Public License (>=v2). 
 *               Read the file COPYING that comes with GRASS
 *               for details.
 *
 **************************************************************/
#include <stdlib.h>
#include "gis.h"
#include "Vect.h"
#include "proto.h"

int nodes ( char *in, char *out)
{
    int    i, node, nnodes, line, nlines, count, type, found;
    double x, y, z;
    struct Map_info In, Out;
    char   *mapset;
    struct line_pnts *Points, *Pout;
    struct line_cats *Cats;

    mapset = G_find_vector2 (in, "");
    if (mapset == NULL) 
        G_fatal_error( "Vector file [%s] not available", in );

    Vect_set_open_level (2);
    Vect_open_old (&In, in, mapset);

    Vect_set_fatal_error (GV_FATAL_PRINT);
    if (1 > Vect_open_new (&Out, out, In.head.with_z)){
        Vect_close (&In);
	G_fatal_error ("Failed opening output vector file");
    }

    Vect_copy_head_data (&In, &Out);
    Vect_hist_copy (&In, &Out);
    Vect_hist_command ( &Out );

    Points = Vect_new_line_struct ();
    Pout = Vect_new_line_struct ();
    
    Cats = Vect_new_cats_struct ();
    
    /* Rewrite all primitives to output file */
    while ( (type = Vect_read_next_line (&In, Points, Cats)) >= 0) {
        Vect_write_line (&Out, type, Points, Cats);      
    }
    
    /* Go thorough all nodes in old map and write a new point if missing */    
    nnodes = Vect_get_num_nodes ( &In );
    Vect_reset_cats (Cats);
    count = 0;
    for ( node = 1; node <= nnodes; node++ ) {
	int has_lines = 0;
	
	nlines = Vect_get_node_n_lines ( &In, node );
	found = 0;
	for ( i = 0; i < nlines; i++ ) {
	    line = abs ( Vect_get_node_line ( &In, node, i ) );
            type = Vect_read_line (&In, NULL, NULL, line);
	    if ( type == GV_POINT ) {
		found = 1;
	    }
	    if ( type & GV_LINES ) {
		has_lines = 1;
	    }
	}
	if ( has_lines && !found ) { /* Write new point */
	    Vect_reset_line ( Pout );    
	    Vect_get_node_coor ( &In, node, &x, &y, &z);
	    Vect_append_point ( Pout, x, y, z);
	    Vect_write_line ( &Out, GV_POINT, Pout, Cats);
	    count++;
	}
    }
    fprintf (stderr, "%d new points written to output.\n", count );
    
    Vect_destroy_line_struct (Points);
    Vect_destroy_line_struct (Pout);
    Vect_destroy_cats_struct (Cats);

    Vect_copy_tables ( &In, &Out, 0 );
    
    /* Support */    
    Vect_build (&Out, stdout); 
    
    Vect_close (&In);
    Vect_close (&Out);
    
    return 0;
}
