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

/* Snap vertex to line */
int 
svtl ( struct Map_info *Out, int otype, int tool, double thresh, int x_flag )
{
	struct line_pnts *APoints, *BPoints, *Points;
	struct line_cats *ACats, *BCats, *Cats;
        int    i, v, k, ret, atype, btype, bline;
	int    nlines, snap;
	int    nsnaps, seg;
	double dist, x, y, px, py;

	/* TODO: snap to itself, 3D */
        APoints = Vect_new_line_struct ();
        BPoints = Vect_new_line_struct ();
        Points = Vect_new_line_struct ();
	ACats = Vect_new_cats_struct ();
	BCats = Vect_new_cats_struct ();
	Cats = Vect_new_cats_struct ();
	
	nlines = Vect_get_num_lines (Out);

        G_debug (1, "nlines =  %d", nlines );
	/* Go through all lines in vector, for each vertex find nearest line, 
	*  if ditance to this line is in threshold snap to that line and optionaly create new vertex 
	*  at this point on that line */
	nsnaps = 0;
	fprintf (stderr, "Snaps: %5d", nsnaps ); 
	for ( i = 1; i <= nlines; i++ ){ 
	    snap = 0;
            G_debug (1, "i =  %d", i);
	    if ( !Vect_line_alive ( Out, i ) ) continue;

	    atype = Vect_read_line (Out, APoints, ACats, i);
	    if ( !(atype & otype) ) continue;

	    for ( v = 0; v <  APoints->n_points; v++ ){ /* for each vertex */
		G_debug (1, "  vertex = %d", v);
		x = APoints->x[v];
		y = APoints->y[v];
		bline = Vect_find_line ( Out, x, y, 0, otype, thresh, 0, i );
		G_debug (1, "  bline = %d", bline);
		if ( bline == 0 ) continue;
		
		btype = Vect_read_line (Out, BPoints, BCats, bline);

		seg = Vect_line_distance ( BPoints, x, y, 0, 0, &px, &py, NULL, &dist, NULL, NULL);
		
		G_debug ( 1, "  dist = %f", dist);
		if ( dist > thresh ) continue;
		
		/* If dist == 0 then the vertex is either on vertex of B line or (almost) exactly
		*  at the line. */
		if ( dist == 0 ) {
		    if ( ( x == BPoints->x[seg-1] && y == BPoints->y[seg-1] ) ||
		         ( x == BPoints->x[seg] && y == BPoints->y[seg] ) ) { 
		        G_debug ( 1, "  identical vertices");
			continue; /* identical vertices (no need to snap) */
		    }
		}
		
		/* Snap vertex of line A */
		APoints->x[v] = px;
		APoints->y[v] = py;
		
		G_debug (1, "Snap line %d vertex  %d : %f, %f", i, v, x, y);
		G_debug (1, "  to line %d segment %d : %f, %f", bline, seg, px, py);
		if ( !x_flag ) {
		    ret = Vect_rewrite_line ( Out, i, atype, APoints, ACats );  
		} else { /* intersection points only */
		    Vect_reset_line ( Points );
		    if ( k > 0 ) {
			Vect_append_point ( Points, px, py, 0 );                            
			ret = Vect_write_line ( Out, GV_POINT, Points, Cats );
		    }
		}	

		/* Add vertex to Bline */
		if ( !x_flag ) {
		    Vect_reset_line ( Points );
		    Vect_copy_xyz_to_pnts ( Points, BPoints->x, BPoints->y, BPoints->z, seg); 
		    Vect_append_point ( Points, px, py, 0 );
		    for ( k = seg; k <  BPoints->n_points; k++ ) {
			Vect_append_point ( Points, BPoints->x[k], BPoints->y[k], BPoints->z[k] );
		    }
		    Vect_delete_line (Out, bline); 
		    ret = Vect_write_line ( Out, btype, Points, BCats );  
		}

		snap = 1;
		nsnaps++;
	        break;	
	    }
		
	    nlines = Vect_get_num_lines (Out);
	    G_debug (3, "nlines =  %d\n", nlines );
	    fprintf (stderr, "\rSnaps: %5d  (line = %d)", nsnaps, i ); 
	    fflush ( stderr );
	}
	fprintf (stderr, "\rSnaps: %5d                 ", nsnaps); 
	fprintf (stderr, "\n" ); 

	return 1;
}

