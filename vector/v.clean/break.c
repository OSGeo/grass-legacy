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
break_lines ( struct Map_info *Out, int otype, int x_flag )
{
	struct line_pnts *APoints, *BPoints, *Points;
        struct line_pnts **AXLines, **BXLines;
	struct line_cats *ACats, *BCats, *Cats;
        int    i, j, k, ret, atype, btype, bline;
	int    nlines, naxlines, nbxlines;
	BOUND_BOX  ABox; 
	struct ilist *List; 
	int nbreaks;

        APoints = Vect_new_line_struct ();
        BPoints = Vect_new_line_struct ();
        Points = Vect_new_line_struct ();
	ACats = Vect_new_cats_struct ();
	BCats = Vect_new_cats_struct ();
	Cats = Vect_new_cats_struct ();
	List = Vect_new_list ();
	
	nlines = Vect_get_num_lines (Out);

        G_debug (1, "nlines =  %d", nlines );
	/* Go through all lines in vector, for each select lines which overlap MBR of
	*  this line and try to intersect, if lines intersect write new lines at the end of 
	*  the file, and process next line (remainining lines overlaping box are skipped) */
	nbreaks = 0;
	fprintf (stderr, "Intersections: %5d", nbreaks ); 
	for ( i = 1; i <= nlines; i++ ){ 
            G_debug (1, "i =  %d", i);
	    if ( !Vect_line_alive ( Out, i ) ) continue;

	    atype = Vect_read_line (Out, APoints, ACats, i);
	    if ( !(atype & otype) ) continue;

	    Vect_line_box ( APoints, &ABox ); 
	    Vect_select_lines_by_box ( Out, &ABox, otype, List);
            G_debug (1, "  %d lines selected by box", List->n_values);
	    
	    for ( j = 0; j <  List->n_values; j++ ){ 
		bline = List->value[j];
                G_debug (1, "  j = %d bline = %d", j, bline);
		if ( i == bline ) continue; /* TODO solve also self intersection */
	
	        btype = Vect_read_line (Out, BPoints, BCats, bline);
	
	        Vect_line_intersection(APoints, BPoints, &AXLines, &BXLines, &naxlines, &nbxlines, 0);
                G_debug(1, "  naxlines = %d nbxlines = %d",  naxlines, nbxlines);
		
		if ( naxlines > 0 ) { /* intersection -> write out */
		    Vect_delete_line (Out, i); 
		    for ( k = 0; k < naxlines; k++ ){ 
			if ( !x_flag ) {
			    ret = Vect_write_line ( Out, atype, AXLines[k], ACats );  
                            G_debug (3, "Line %d written", ret);
			} else { /* intersection points only */
			    Vect_reset_line ( Points );
			    if ( k > 0 ) {
				Vect_append_point ( Points, AXLines[k]->x[0], AXLines[k]->y[0], AXLines[k]->z[0] );                            
				ret = Vect_write_line ( Out, GV_POINT, Points, Cats );
			    }
			}	
			Vect_destroy_line_struct (  AXLines[k] );
		    }
		    G_free ( AXLines );
		    nbreaks += naxlines - 1;
		}
		    
		if ( nbxlines > 0 ) { 
		    Vect_delete_line (Out, bline); 
		    for ( k = 0; k < nbxlines; k++ ){ 
			if ( !x_flag ) {
			    ret = Vect_write_line ( Out, btype, BXLines[k], BCats );  
                            G_debug (3, "Line %d written", ret);
			} else { /* intersection points only */
			    Vect_reset_line ( Points );
			    if ( k > 0 ) {
				Vect_append_point ( Points, BXLines[k]->x[0], BXLines[k]->y[0], BXLines[k]->z[0] );                            
				ret = Vect_write_line ( Out, GV_POINT, Points, Cats );
			    }
			}	
			Vect_destroy_line_struct (  BXLines[k] );
		    }
		    G_free ( BXLines );
		    nbreaks += nbxlines - 1;
		}	
		
		fprintf (stderr, "\rIntersections: %5d", nbreaks ); 
		fflush ( stderr );
		if ( naxlines > 0 ) break; /* first line was broken and deleted -> take the next one */
	    }
	    nlines = Vect_get_num_lines (Out);
	    G_debug (3, "nlines =  %d\n", nlines );
	}
	fprintf (stderr, "\n" ); 

	return 1;
}

