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

/* Pruning of boundaries MUST NOT destroy topology of areas. This is guaranteed by 3 rules:
 *
 * 1) first and lst segment of the boundary is never changed
 * 
 * 2) if pruned boundary would cross another boundary, pruning is not done
 *    (original boundary is left unchanged)
 *
 * 3) position of centroids on the left and right side is checked and pruning
 *    is not done if centroid would be attached to another area 
 */

int 
prune ( struct Map_info *Out, int otype, double thresh )
{
        int line, type, nlines;
	int nremoved = 0; /* number of removed vertices */
	int nvertices = 0; /* number of input vertices in given type */
	int not_pruned_lines = 0; /* number of not pruned because of topology */
	int norig;
	struct line_pnts *Points, *TPoints, *BPoints;
        struct line_cats *Cats;
	BOUND_BOX box;
	struct ilist *List;

        Points = Vect_new_line_struct ();
        TPoints = Vect_new_line_struct ();
        BPoints = Vect_new_line_struct ();
        Cats = Vect_new_cats_struct ();
	List = Vect_new_list ();
	
	nlines = Vect_get_num_lines (Out);

        G_debug (1, "nlines =  %d", nlines );

	fprintf (stderr, "Removed vertices: %5d", nremoved ); 
	for ( line = 1; line <= nlines; line++ ){ 
	    
	    if ( !Vect_line_alive ( Out, line ) ) continue;

	    type = Vect_read_line (Out, Points, Cats, line);
	    if ( !(type & otype & GV_LINES) ) continue;

            G_debug (3, "line = %d n_point = %d", line, Points->n_points );

	    norig = Points->n_points;
	    nvertices += Points->n_points;

	    if ( type == GV_LINE ) {
		Vect_line_prune_thresh ( Points, thresh );

		if ( Points->n_points < norig ) {
		    Vect_rewrite_line ( Out, line, type, Points, Cats );
		    nremoved += norig - Points->n_points;
		}
		    
	    } else if ( type == GV_BOUNDARY ) {
		int i, intersect, newline, left_old, right_old, left_new, right_new;
		
		if ( norig < 5 ) continue; /* Nothing can be removed */
		
		/* Make a copy of points without first and last segment */
		Vect_reset_line ( TPoints );

		for ( i = 1; i < norig-1; i++ ) {
		    Vect_append_point ( TPoints, Points->x[i], Points->y[i], Points->z[i] );
		}

		Vect_line_prune_thresh ( TPoints, thresh );

		if ( TPoints->n_points == norig-2 ) continue; /* no pruning done */

		/* Append first and last point */
		Vect_line_insert_point ( TPoints, 0,  Points->x[0], Points->y[0], Points->z[0] );
		Vect_append_point ( TPoints, Points->x[norig-1], Points->y[norig-1], Points->z[norig-1] );

		/* Check intersection of the pruned boundary with other boundaries */
		Vect_line_box ( TPoints, &box );
		Vect_select_lines_by_box ( Out, &box, GV_BOUNDARY, List);

		intersect = 0;
		for ( i = 0; i <  List->n_values; i++ ) {
		    int j, bline;
		    struct line_pnts **AXLines, **BXLines;
		    int naxlines, nbxlines;
		    
		    bline = List->value[i];
		    if ( bline == line ) continue;
		    
		    Vect_read_line ( Out, BPoints, NULL, bline );

		    /* Vect_line_intersection is quite slow, hopefully not so bad because only few 
		     * intersections should be found if any */

		    Vect_line_intersection ( TPoints, BPoints, &AXLines, &BXLines, &naxlines, &nbxlines, 0);
		    
		    G_debug ( 4, "bline = %d intersect = %d naxlines = %d nbxlines = %d", 
			         bline, intersect, naxlines, nbxlines );

		    /* Free */
		    if ( naxlines > 0 ) {
		        for ( j = 0; j < naxlines; j++ ) {
			    Vect_destroy_line_struct (  AXLines[j] );
			}
			G_free ( AXLines );
		    }
		    if ( nbxlines > 0 ) {
		        for ( j = 0; j < nbxlines; j++ ) {
			    Vect_destroy_line_struct (  BXLines[j] );
			}
			G_free ( BXLines );
		    }

		    if ( naxlines > 0 ||  nbxlines > 0 ) {
			intersect =  1;
			break;
		    }
		}

		if ( intersect ) {
		    G_debug ( 3, "The pruned boundary instersects another boundary -> not pruned" );
		    not_pruned_lines++;
		    continue;
		}

		/* Get centroids on the left and right side */
		Vect_get_line_areas ( Out, line, &left_old, &right_old );
		if ( left_old < 0 ) left_old = Vect_get_isle_area ( Out, abs(left_old) );
                if ( left_old > 0 ) left_old = Vect_get_area_centroid ( Out, left_old );
		if ( right_old < 0 ) right_old = Vect_get_isle_area ( Out, abs(right_old) );
                if ( right_old > 0 ) right_old = Vect_get_area_centroid ( Out, right_old );

		/* OK, rewrite pruned */
		newline = Vect_rewrite_line ( Out, line, type, TPoints, Cats );

		/* Check position of centroids */
		Vect_get_line_areas ( Out, newline, &left_new, &right_new );
		if ( left_new < 0 ) left_new = Vect_get_isle_area ( Out, abs(left_new) );
                if ( left_new > 0 ) left_new = Vect_get_area_centroid ( Out, left_new );
		if ( right_new < 0 ) right_new = Vect_get_isle_area ( Out, abs(right_new) );
                if ( right_new > 0 ) right_new = Vect_get_area_centroid ( Out, right_new );

		if ( left_new != left_old || right_new != right_old ) {
		    G_debug ( 3, "The pruned boundary changes attachement of centroid -> not pruned" );
		    Vect_rewrite_line ( Out, newline, type, Points, Cats );
		    not_pruned_lines++;
		    continue;
		}

		nremoved += norig - TPoints->n_points;
		G_debug ( 4, "%d vertices removed", norig - TPoints->n_points );
	    }
		
	    fprintf (stderr, "\rRemoved vertices: %5d", nremoved ); 
	    fflush ( stderr );
	}
	fprintf (stderr, "\n" ); 
	fprintf (stderr, "%d vertices from input %d (vertices of given type) removed, i.e. %.2f %%\n",
	                 nremoved, nvertices, 100.0*nremoved/nvertices ); 

	if ( not_pruned_lines > 0 )
	    fprintf (stderr, "%d boundaries not pruned because pruning would damage topology.\n", 
		              not_pruned_lines);

	return 1;
}


