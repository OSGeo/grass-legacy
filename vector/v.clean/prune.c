/* ***************************************************************
 * *
 * * MODULE:       v.clean
 * * 
 * * AUTHOR(S):    Radim Blazek
 * *               
 * * PURPOSE:      Clean lines
 * *               
 * * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 * *
 * *               This program is free software under the 
 * *               GNU General Public License (>=v2). 
 * *               Read the file COPYING that comes with GRASS
 * *               for details.
 * *
 * **************************************************************/
#include <stdlib.h> 
#include "gis.h"
#include "Vect.h"

int 
prune ( struct Map_info *Out, int otype, double thresh )
{
        int line, type, nlines;
	int nremoved = 0; /* number of removed vertices */
	int nvertices = 0; /* number of input vertices in given type */
	int norig;
	struct line_pnts *Points;
        struct line_cats *Cats;

        Points = Vect_new_line_struct ();
        Cats = Vect_new_cats_struct ();
	
	nlines = Vect_get_num_lines (Out);

        G_debug (1, "nlines =  %d", nlines );

	fprintf (stderr, "Removed vertices: %5d", nremoved ); 
	for ( line = 1; line <= nlines; line++ ){ 
	    if ( !Vect_line_alive ( Out, line ) ) continue;

	    type = Vect_read_line (Out, Points, Cats, line);
	    if ( !(type & otype & GV_LINES) ) continue;

	    norig = Points->n_points;
	    nvertices += Points->n_points;

	    Vect_line_prune_thresh ( Points, thresh );

	    if ( Points->n_points < norig ) {
		Vect_rewrite_line ( Out, line, type, Points, Cats );
		nremoved += norig - Points->n_points;
		
		fprintf (stderr, "\rRemoved vertices: %5d", nremoved ); 
		fflush ( stderr );
	    }
	}
	fprintf (stderr, "\n" ); 
	fprintf (stderr, "%d vertices from input %d (vertices of given type) removed, i.e. %.2f %%\n",
	                 nremoved, nvertices, 100.0*nremoved/nvertices ); 

	return 1;
}


