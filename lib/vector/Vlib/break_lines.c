/***************************************************************
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
 \fn void Vect_break_lines ( struct Map_info *Map, int type, struct Map_info *Err, FILE *msgout)
 \brief Break lines in vector map.

 Breaks lines specified by type in vector map. Points at intersections may be optionaly 
 written to error map. Input map must be opened on level 2 for update

 \param Map input map where lines will be broken
 \param type type of line to be broken
 \param Err vector map where points at intersections will be written or NULL
 \param msgout file pointer where messages will be written or NULL
 \return
*/
void 
Vect_break_lines ( struct Map_info *Map, int type, struct Map_info *Err, FILE *msgout )
{
    struct line_pnts *APoints, *BPoints, *Points;
    struct line_pnts **AXLines, **BXLines;
    struct line_cats *ACats, *BCats, *Cats;
    int    i, j, k, l, ret, atype, btype, bline, found;
    int    nlines, naxlines, nbxlines, nx;
    double *xx, *yx, *zx;
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
    
    nlines = Vect_get_num_lines (Map);

    G_debug (3, "nlines =  %d", nlines );
    /* Go through all lines in vector, for each select lines which overlap MBR of
    *  this line and try to intersect, if lines intersect write new lines at the end of 
    *  the file, and process next line (remainining lines overlaping box are skipped) */
    nbreaks = 0;
    if (msgout) fprintf (msgout, "Intersections: %5d", nbreaks ); 
    for ( i = 1; i <= nlines; i++ ){ 
	G_debug (4, "i =  %d", i);
	if ( !Vect_line_alive ( Map, i ) ) continue;

	atype = Vect_read_line (Map, APoints, ACats, i);
	if ( !(atype & type) ) continue;

	Vect_line_box ( APoints, &ABox ); 
	Vect_select_lines_by_box ( Map, &ABox, type, List);
	G_debug (4, "  %d lines selected by box", List->n_values);
	
	for ( j = 0; j <  List->n_values; j++ ){ 
	    bline = List->value[j];
	    G_debug (5, "  j = %d bline = %d", j, bline);
    
	    btype = Vect_read_line (Map, BPoints, BCats, bline);
    
	    Vect_line_intersection(APoints, BPoints, &AXLines, &BXLines, &naxlines, &nbxlines, 0);
	    G_debug(5, "  naxlines = %d nbxlines = %d",  naxlines, nbxlines);
	    
	    if ( Err ) { /* array for intersections (more than needed */
		xx = (double *) G_malloc ( (naxlines + nbxlines) * sizeof ( double ) ); 
		yx = (double *) G_malloc ( (naxlines + nbxlines) * sizeof ( double ) ); 
		zx = (double *) G_malloc ( (naxlines + nbxlines) * sizeof ( double ) ); 
	    }
	    nx = 0; /* number of intersections to be written to Err */
	    if ( naxlines > 0 ) { /* intersection -> write out */
		Vect_delete_line (Map, i); 
		for ( k = 0; k < naxlines; k++ ){ 
		    /* Write new line segments */
		    ret = Vect_write_line ( Map, atype, AXLines[k], ACats );  
		    G_debug (5, "Line %d written", ret);
		    
		    /* Write intersection points */
		    if ( Err ) {
			if ( k > 0 ) {
			    xx[nx] = AXLines[k]->x[0];
			    yx[nx] = AXLines[k]->y[0];
			    zx[nx] = AXLines[k]->z[0];
			    nx++;
			}
		    }	
		    Vect_destroy_line_struct (  AXLines[k] );
		}
		G_free ( AXLines );
		nbreaks += naxlines - 1;
	    }
		
	    if ( nbxlines > 0 ) { 
		if ( i != bline ) { /* Self intersection, do not write twice, TODO: is it OK? */
		    Vect_delete_line (Map, bline); 
		    for ( k = 0; k < nbxlines; k++ ){ 
		        /* Write new line segments */
			ret = Vect_write_line ( Map, btype, BXLines[k], BCats );  
			G_debug (5, "Line %d written", ret);
			
			/* Write intersection points */
			if ( Err ) {
			    if ( k > 0 ) {
				found = 0;
				for ( l = 0; l < nx; l++ ) {
				    if ( xx[l] == BXLines[k]->x[0] && yx[l] == BXLines[k]->y[0] &&
				         zx[l] == BXLines[k]->z[0] )
				    {
					found = 1;
					break;
				    }
				}
				if ( !found ) {
				    xx[nx] = BXLines[k]->x[0];
				    yx[nx] = BXLines[k]->y[0];
				    zx[nx] = BXLines[k]->z[0];
				    nx++;
				}
			    }
			}	
		        Vect_destroy_line_struct (  BXLines[k] );
		    }
		    nbreaks += nbxlines - 1;
		} else {
		    for ( k = 0; k < nbxlines; k++ ) 
			Vect_destroy_line_struct (  BXLines[k] );
		}
		G_free ( BXLines );
	    }	
	    if ( Err ) {
		for ( l = 0; l < nx; l++ ) { /* Write out errors */
	            Vect_reset_line ( Points );
		    Vect_append_point ( Points, xx[l], yx[l], zx[l] );                            
		    ret = Vect_write_line ( Err, GV_POINT, Points, Cats );
		}
		
		G_free ( xx );
		G_free ( yx );
		G_free ( zx );
	    }
	    
	    if (msgout) fprintf (msgout, "\rIntersections: %5d", nbreaks ); 
	    if (msgout) fflush ( msgout );
	    if ( naxlines > 0 ) break; /* first line was broken and deleted -> take the next one */
	}
	nlines = Vect_get_num_lines (Map);
	G_debug (3, "nlines =  %d\n", nlines );
    }
    if (msgout) fprintf (msgout, "\n" ); 
}

