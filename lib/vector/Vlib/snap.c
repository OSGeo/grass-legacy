/**************************************************************
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
 \fn void Vect_snap_vertices ( struct Map_info *Map, int type, double thresh, 
                              struct Map_info *Err, FILE *msgout)
 \brief Snap vertices of line to other lines.

 Snap vertices of line to other lines in given threshold. On the line where vertex
 was snapped is created new vertex with identical coordinates.
 Lines showing how vertices were snapped may be optionaly written to error map. 
 Input map must be opened on level 2 for update.

 \param Map input map where verices will be snapped
 \param type type of line to be snap
 \param thresh threshold in which snap vertices
 \param Err vector map where lines representing snap are written or NULL
 \param msgout file pointer where messages will be written or NULL
 \return
*/
void 
Vect_snap_vertices ( struct Map_info *Map, int type, double thresh, struct Map_info *Err, FILE *msgout )
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
    
    nlines = Vect_get_num_lines (Map);

    G_debug (1, "nlines =  %d", nlines );
    /* Go through all lines in vector, for each vertex find nearest line, 
    *  if ditance to this line is in threshold snap to that line and optionaly create new vertex 
    *  at this point on that line */
    nsnaps = 0;
    if ( msgout ) fprintf (msgout, "Snaps: %5d", nsnaps ); 
    for ( i = 1; i <= nlines; i++ ){ 
	snap = 0;
	G_debug (1, "i =  %d", i);
	if ( !Vect_line_alive ( Map, i ) ) continue;

	atype = Vect_read_line (Map, APoints, ACats, i);
	if ( !(atype & type) ) continue;

	for ( v = 0; v <  APoints->n_points; v++ ){ /* for each vertex */
	    G_debug (1, "  vertex = %d", v);
	    x = APoints->x[v];
	    y = APoints->y[v];
	    bline = Vect_find_line ( Map, x, y, 0, type, thresh, 0, i );
	    G_debug (1, "  bline = %d", bline);
	    if ( bline == 0 ) continue;
	    
	    btype = Vect_read_line (Map, BPoints, BCats, bline);

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

	    /* Write to Err line connecting original and new coordinates */
	    if ( Err ) {
		Vect_reset_line ( Points );
		Vect_append_point ( Points, APoints->x[v], APoints->y[v], 0 ); 
		Vect_append_point ( Points, px, py, 0 );            
		Vect_write_line ( Err, GV_LINE, Points, Cats );
	    }
		
	    
	    /* Snap vertex of line A */
	    APoints->x[v] = px;
	    APoints->y[v] = py;
	    
	    G_debug (1, "Snap line %d vertex  %d : %f, %f", i, v, x, y);
	    G_debug (1, "  to line %d segment %d : %f, %f", bline, seg, px, py);
	    
	    ret = Vect_rewrite_line ( Map, i, atype, APoints, ACats );  

	    /* Add vertex to Bline */
	    Vect_reset_line ( Points );
	    Vect_copy_xyz_to_pnts ( Points, BPoints->x, BPoints->y, BPoints->z, seg); 
	    Vect_append_point ( Points, px, py, 0 );
	    for ( k = seg; k <  BPoints->n_points; k++ ) {
		Vect_append_point ( Points, BPoints->x[k], BPoints->y[k], BPoints->z[k] );
	    }
	    Vect_delete_line (Map, bline); 
	    ret = Vect_write_line ( Map, btype, Points, BCats );  

	    snap = 1;
	    nsnaps++;
	    break;	
	}
	    
	nlines = Vect_get_num_lines (Map);
	G_debug (3, "nlines =  %d\n", nlines );
	if ( msgout ) {
  	    fprintf (msgout, "\rSnaps: %5d  (line = %d)", nsnaps, i ); 
	    fflush ( msgout );
	}
    }
    if ( msgout ) {
        fprintf (msgout, "\rSnaps: %5d                 ", nsnaps); 
        fprintf (msgout, "\n" ); 
    }
}

