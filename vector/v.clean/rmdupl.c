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
rmdupl ( struct Map_info *Out, int otype )
{
	struct line_pnts *APoints, *BPoints;
	struct line_cats *ACats, *BCats, *Cats;
        int    i, j, k, atype, btype, bline;
	int    nlines, npoints;
	BOUND_BOX  ABox; 
	struct ilist *List; 
	int ndupl;
	int forw, backw;

	
        APoints = Vect_new_line_struct ();
        BPoints = Vect_new_line_struct ();
	ACats = Vect_new_cats_struct ();
	BCats = Vect_new_cats_struct ();
	Cats = Vect_new_cats_struct ();
	List = Vect_new_list ();
	
	nlines = Vect_get_num_lines (Out);

        G_debug (1, "nlines =  %d", nlines );
	/* Go through all lines in vector, for each select lines which overlap MBR of
	*  this line and check if some of them is identical. If someone is identical
	*  remove current line. (In each step just one line is deleted)
	*/
        /* TODO: Categories, 3D */
	ndupl = 0;
	fprintf (stderr, "Duplicates: %5d", ndupl ); 
	for ( i = 1; i <= nlines; i++ ){ 
	    if ( !Vect_line_alive ( Out, i ) ) continue;

	    atype = Vect_read_line (Out, APoints, ACats, i);
	    if ( !(atype & otype) ) continue;

	    Vect_line_box ( APoints, &ABox ); 
	    Vect_select_lines_by_box ( Out, &ABox, otype, List);
            G_debug (3, "  %d lines selected by box", List->n_values);
	    
	    for ( j = 0; j <  List->n_values; j++ ){ 
		bline = List->value[j];
                G_debug (1, "  j = %d bline = %d", j, bline);
		if ( i == bline ) continue; 
	
		
	        btype = Vect_read_line (Out, BPoints, BCats, bline);
	
		/* Check if the lines are identical */
		if ( APoints->n_points != BPoints->n_points ) continue;

		npoints = APoints->n_points;
		/* Forward */
		forw = 1;
	        for ( k = 0; k <  APoints->n_points; k++ ){ 
                    if ( APoints->x[k] != BPoints->x[k] || APoints->y[k] != BPoints->y[k] ) {
                        forw = 0; break;
		    }	
		}
		
		/* Backward */
		backw = 1;
	        for ( k = 0; k <  APoints->n_points; k++ ){ 
                    if ( APoints->x[k] != BPoints->x[npoints - k - 1] || 
			 APoints->y[k] != BPoints->y[npoints - k - 1] ) {
                        backw = 0; break;
		    }	
		}
		
		if ( !forw && !backw ) continue;

		/* Lines area identical -> remove current */
		Vect_delete_line (Out, i); 
		ndupl++;
		
		fprintf (stderr, "\rDuplicates: %5d", ndupl ); 
		fflush ( stderr );
		
		break; /* line was deleted -> take the next one */
	    }
	    nlines = Vect_get_num_lines (Out); /* For future when lines with cats will be rewritten */
	    G_debug (3, "nlines =  %d\n", nlines );
	}
	fprintf (stderr, "\n" ); 

	return 1;
}


