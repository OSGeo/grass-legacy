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
#include <math.h>
#include "gis.h"
#include "Vect.h"

typedef struct {
    double x, y;
    double a1, a2; /* angles */
    int cross; /* 0 - do not break, 1 - break */
    int used; /* 0 - was not used to break line, 1 - was used to break line
	       *   this is stored because points are automaticaly marked as cross, even if not used 
	       *   later to break lines */	 
} XPNT;

static int fpoint;
/* Function called from RTreeSearch for point found */
void srch (int id, int *arg)
{
    fpoint = id;
}

/*!
 \fn void Vect_break_polygons ( struct Map_info *Map, int type, struct Map_info *Err, FILE *msgout)
 \brief Break polygons in vector map.

 Breaks lines specified by type in vector map. Points at intersections may be optionaly 
 written to error map. Input map must be opened on level 2 for update. Function is 
 optimized for closed polygons rigs (e.g. imported from OGR) but with clean geometry -
 adjacent polygons mostly have identical boundary. Function creates database of ALL points
 in the map, and then is looking for those where polygons should be broken. 
 Lines may be broken only at points existing in input map! 

 \param Map input map where polygons will be broken
 \param type type of line to be broken
 \param Err vector map where points at intersections will be written or NULL
 \param msgout file pointer where messages will be written or NULL
 \return
*/
void 
Vect_break_polygons ( struct Map_info *Map, int type, struct Map_info *Err, FILE *msgout )
{
    struct line_pnts *BPoints, *Points;
    struct line_cats *Cats;
    int    i, j, k, ret, ltype, broken, last, nlines;
    int nbreaks;
    struct Node *RTree;
    int    apoints, npoints, nallpoints, nmarks;
    XPNT   *XPnts;
    struct Rect rect;
    double dx, dy, a1, a2;

    RTree = RTreeNewIndex();

    BPoints = Vect_new_line_struct ();
    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    
    nlines = Vect_get_num_lines (Map);

    G_debug (3, "nlines =  %d", nlines );
    /* Go through all lines in vector, and add each point to structure of points,
     * if such point already exists check angles of segments and if differ mark for break */

    apoints = 0;
    nmarks = 0;
    npoints = 1; /* index starts from 1 !*/
    nallpoints = 0;
    XPnts = NULL;
    

    if ( msgout ) fprintf (msgout, "Registerd points: %9d", npoints - 1 ); 
    for ( i = 1; i <= nlines; i++ ){ 
	G_debug (3, "i =  %d", i);
	if ( !Vect_line_alive ( Map, i ) ) continue;

	ltype = Vect_read_line (Map, Points, Cats, i);
	if ( !(ltype & type) ) continue;

	for ( j = 0; j <  Points->n_points; j++ ){ 
	    G_debug (3, "j =  %d", j);
	    nallpoints++;

            /* Box */
            rect.boundary[0] = Points->x[j];  rect.boundary[3] = Points->x[j];	    
            rect.boundary[1] = Points->y[j];  rect.boundary[4] = Points->y[j];	    
            rect.boundary[2] = 0;  rect.boundary[5] = 0;	    

	    /* Already in DB? */
	    fpoint = -1;
	    RTreeSearch(RTree, &rect, (void *)srch, 0);
	    G_debug (3, "fpoint =  %d", fpoint);

	    /* Angles of first and last point set simply to zero and mark for cross 
	     * (probably could be done better) */
            if ( j != 0 && j != (Points->n_points - 1) && Points->n_points > 2 ) {
	        dx = Points->x[j-1] - Points->x[j]; dy = Points->y[j-1] - Points->y[j]; 
		a1 = atan2 ( dy, dx );
		dx = Points->x[j+1] - Points->x[j]; dy = Points->y[j+1] - Points->y[j]; 
		a2 = atan2 ( dy, dx );
	    }

	    if ( fpoint >= 0 ) { /* Found */
		if ( XPnts[fpoint].cross == 1 ) continue; /* already marked */
		
		/* Check angles */
		if ( j == 0 || j == (Points->n_points - 1) || Points->n_points < 3 ) {
		    XPnts[fpoint].cross = 1;
		    nmarks++;
		} else { 
	            G_debug (3, "a1 = %f xa1 = %f a2 = %f xa2 = %f", a1, XPnts[fpoint].a1,
			                a2, XPnts[fpoint].a2);
		    if ( ( a1 == XPnts[fpoint].a1 && a2 == XPnts[fpoint].a2 ) ||
		         ( a1 == XPnts[fpoint].a2 && a2 == XPnts[fpoint].a1 ) ) 
		    { /* identical */

		    } else {
                        XPnts[fpoint].cross = 1;
			nmarks++;
		    }
		}
	    
	    } else { 
		/* Add to tree and to structure */
	        RTreeInsertRect( &rect, npoints, &RTree, 0);
	        if ( (npoints - 1) == apoints ) {
		    apoints += 100;
		    XPnts = (XPNT *) G_realloc ( XPnts, (apoints + 1) * sizeof (XPNT) );
		}
		XPnts[npoints].x = Points->x[j];
		XPnts[npoints].y = Points->y[j];
		XPnts[npoints].used = 0;
		if ( j == 0 || j == (Points->n_points - 1) || Points->n_points < 3 ) {
		    XPnts[npoints].a1 = 0; 
		    XPnts[npoints].a2 = 0; 
		    XPnts[npoints].cross = 1;
		    nmarks++;
		} else { 
		    XPnts[npoints].a1 = a1;
		    XPnts[npoints].a2 = a2;
		    XPnts[npoints].cross = 0;
		}
		    
                npoints++;		    
            }
	    if ( msgout ) {
		fprintf (msgout, "\b\b\b\b\b\b\b\b\b%9d", npoints - 1 ); 
	        fflush ( msgout );
	    }
	}
    }
    if ( msgout ) {
	fprintf ( msgout, "\n" ); 
	fprintf ( msgout, "All points (vertices): %5d\n", nallpoints ); 
	fprintf ( msgout, "Registered points (unique coordinates): %5d\n", npoints - 1 ); 
	fprintf ( msgout, "Points marked for break: %5d\n", nmarks ); 
    }

    //sleep (10);
    
    nbreaks = 0;
    if ( msgout ) fprintf (msgout, "Breaks: %5d", nbreaks ); 

    /* Second loop through lines: check if points are marked and break */
    for ( i = 1; i <= nlines; i++ ){ 
	G_debug (3, "i =  %d", i);
	if ( !Vect_line_alive ( Map, i ) ) continue;

	ltype = Vect_read_line (Map, Points, Cats, i);
	if ( !(ltype & type) ) continue;
	if ( !(ltype & GV_LINES ) ) continue; /* Nonsense to break points */

	broken = 0; last = 0;
	G_debug (3, "n_points =  %d", Points->n_points);
	for ( j = 1; j <  Points->n_points; j++ ){ 
	    G_debug (3, "j =  %d", j);
	    nallpoints++;

	    /* Box */
	    rect.boundary[0] = Points->x[j];  rect.boundary[3] = Points->x[j];	    
	    rect.boundary[1] = Points->y[j];  rect.boundary[4] = Points->y[j];	    
	    rect.boundary[2] = 0;  rect.boundary[5] = 0;	    

	    if ( Points->n_points <= 1 || (j == (Points->n_points-1) && !broken) ) break; 
	      /* One point only or 
	       * last point and line is not broken, do nothing */

	    RTreeSearch(RTree, &rect, (void *)srch, 0);
	    G_debug (3, "fpoint =  %d", fpoint);

 	    if ( XPnts[fpoint].cross ) { /* realy use to break line */
		XPnts[fpoint].used = 1;
	    }
		
	    if ( j == (Points->n_points-1) || XPnts[fpoint].cross  ) { 
		/* last point or cross (yes last is always cross currently) */

		if ( !broken ) Vect_delete_line (Map, i); /* not yet deleted */
		
		Vect_reset_line ( BPoints );
		for ( k = last; k <= j; k++ ){ 
		    Vect_append_point ( BPoints, Points->x[k], Points->y[k], Points->z[k] ); 
		}

		ret = Vect_write_line ( Map, ltype, BPoints, Cats );  
		G_debug (3, "Line %d written", ret);
		last = j;
		broken = 1;
		nbreaks++;
	    }
		
	    if ( msgout ) {
		fprintf ( msgout, "\rBreaks: %5d", nbreaks ); 
		fflush ( msgout );
	    }
	}
	nlines = Vect_get_num_lines (Map);
	G_debug (3, "nlines =  %d\n", nlines );
    }

    /* Write points on breaks */
    if ( Err ) {
	Vect_reset_cats ( Cats );
	for ( i = 1; i <= npoints; i++ ){ 
	    if ( XPnts[i].used ) {
		Vect_reset_line ( Points );
		Vect_append_point ( Points, XPnts[i].x, XPnts[i].y, 0 ); 
	        Vect_write_line ( Err, GV_POINT, Points, Cats );
		if ( msgout ) {
		    fprintf (msgout, "\rBreaks: %5d", nbreaks ); 
		    fflush ( msgout );
		}
                nbreaks++;
	    }
	}
    }

    if ( msgout ) fprintf ( msgout, "\n" ); 

    G_free ( XPnts );
    RTreeDestroyNode ( RTree);
}

