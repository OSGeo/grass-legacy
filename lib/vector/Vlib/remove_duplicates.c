/* **************************************************************
 * 
 * MODULE:       vector library
 * 
 * AUTHOR(S):    Radim Blazek
 *               
 * PURPOSE:      Clean lines
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

/*!
 \fn void Vect_remove_duplicates ( struct Map_info *Map, int type, struct Map_info *Err, FILE *msgout)
 \brief Remove duplicate lines from vector map.

 Remove duplicate lines of given types from vector map. Duplicate lines may be optionaly 
 written to error map. Input map must be opened on level 2 for update.

 \param Map input map where duplicate lines will be deleted
 \param type type of line to be delete
 \param Err vector map where duplicate lines will be written or NULL
 \param msgout file pointer where messages will be written or NULL
 \return
*/
void 
Vect_remove_duplicates ( struct Map_info *Map, int type, struct Map_info *Err, FILE *msgout )
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
	
	nlines = Vect_get_num_lines (Map);

        G_debug (1, "nlines =  %d", nlines );
	/* Go through all lines in vector, for each select lines which overlap MBR of
	*  this line and check if some of them is identical. If someone is identical
	*  remove current line. (In each step just one line is deleted)
	*/
        /* TODO: Categories, 3D */
	ndupl = 0;
	if ( msgout ) fprintf (msgout, "Duplicates: %5d", ndupl ); 
	for ( i = 1; i <= nlines; i++ ){ 
	    if ( !Vect_line_alive ( Map, i ) ) continue;

	    atype = Vect_read_line (Map, APoints, ACats, i);
	    if ( !(atype & type) ) continue;

	    Vect_line_box ( APoints, &ABox ); 
	    Vect_select_lines_by_box ( Map, &ABox, type, List);
            G_debug (3, "  %d lines selected by box", List->n_values);
	    
	    for ( j = 0; j <  List->n_values; j++ ){ 
		bline = List->value[j];
                G_debug (3, "  j = %d bline = %d", j, bline);
		if ( i == bline ) continue; 
		
	        btype = Vect_read_line (Map, BPoints, BCats, bline);
	
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
		if ( Err ) {
		    Vect_write_line ( Err, atype, APoints, ACats );
		}

		Vect_delete_line (Map, i); 
		ndupl++;
		
		if ( msgout ) {
		    fprintf (stderr, "\rDuplicates: %5d", ndupl ); 
		    fflush ( stderr );
		}
		
		break; /* line was deleted -> take the next one */
	    }
	    nlines = Vect_get_num_lines (Map); /* For future when lines with cats will be rewritten */
	    G_debug (3, "nlines =  %d\n", nlines );
	}
	if ( msgout ) fprintf (stderr, "\n" ); 

	return;
}

