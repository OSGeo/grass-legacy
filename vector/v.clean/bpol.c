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

typedef struct {
    double x, y;
    double a1, a2; /* angles */
    int cross; /* 0 - do not break, 1 - break */
} XPNT;

static int fpoint;
/* Function called from RTreeSearch for point found */
void srch (int id, int *arg)
{
    fpoint = id;
}
    

int 
break_polygons ( struct Map_info *Out, int otype, int x_flag )
{
    struct line_pnts *BPoints, *Points;
    struct line_cats *Cats;
    int    i, j, k, ret, type, broken, last, nlines;
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
    
    nlines = Vect_get_num_lines (Out);

    G_debug (3, "nlines =  %d", nlines );
    /* Go through all lines in vector, and add each point to structure of points,
     * if such point already exists check angles of segments and if differ mark for break */

    apoints = 0;
    nmarks = 0;
    npoints = 1; /* index starts from 1 !*/
    nallpoints = 0;
    XPnts = NULL;
    fprintf (stderr, "Registerd points: %9d", npoints - 1 ); 
    for ( i = 1; i <= nlines; i++ ){ 
	G_debug (3, "i =  %d", i);
	if ( !Vect_line_alive ( Out, i ) ) continue;

	type = Vect_read_line (Out, Points, Cats, i);
	if ( !(type & otype) ) continue;

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
		if ( j == 0 || j == (Points->n_points - 1) || Points->n_points < 3 ) {
		    XPnts[npoints].a1 = 0; 
		    XPnts[npoints].a2 = 0; 
		    XPnts[npoints].cross = 1;
		    nmarks++;
		} else { 
		    XPnts[npoints].a1 = a1;
		    XPnts[npoints].a2 = a2;
		}
		    
                npoints++;		    
            }
	    fprintf (stderr, "\b\b\b\b\b\b\b\b\b%9d", npoints - 1 ); 
	    fflush ( stderr );
	}
    }
    fprintf (stderr, "\n" ); 
    fprintf (stderr, "All points: %5d\n", nallpoints ); 
    fprintf (stderr, "Registered points: %5d\n", npoints - 1 ); 
    fprintf (stderr, "Points marked for break: %5d\n", nmarks ); 
    
    nbreaks = 0;
    fprintf (stderr, "Intersections: %5d", nbreaks ); 
    /* Write points on breaks only */
    if ( x_flag ) {
	Vect_reset_cats ( Cats );
	for ( i = 1; i <= npoints; i++ ){ 
	    if ( XPnts[i].cross ) {
		Vect_reset_line ( Points );
		Vect_append_point ( Points, XPnts[i].x, XPnts[i].y, 0 ); 
	        Vect_write_line ( Out, GV_POINT, Points, Cats );
	        fprintf (stderr, "\rIntersections: %5d", nbreaks ); 
	        fflush ( stderr );
                nbreaks++;
	    }
	}
    } else { 
	/* Second loop through lines: check if points are marked and break */
	for ( i = 1; i <= nlines; i++ ){ 
	    G_debug (3, "i =  %d", i);
	    if ( !Vect_line_alive ( Out, i ) ) continue;

	    type = Vect_read_line (Out, Points, Cats, i);
	    if ( !(type & otype) ) continue;

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

		if ( j == (Points->n_points-1) || XPnts[fpoint].cross  ) { 
		    /* last point or cross (yes last is always cross at currently) */

		    if ( !broken ) Vect_delete_line (Out, i); /* not yet deleted */
		    
		    Vect_reset_line ( BPoints );
		    for ( k = last; k <= j; k++ ){ 
			Vect_append_point ( BPoints, Points->x[k], Points->y[k], Points->z[k] ); 
		    }

		    ret = Vect_write_line ( Out, type, BPoints, Cats );  
 	            G_debug (3, "Line %d written", ret);
		    last = j;
		    broken = 1;
		    nbreaks++;
		}
		    
		fprintf (stderr, "\rIntersections: %5d", nbreaks ); 
		fflush ( stderr );
	    }
	    nlines = Vect_get_num_lines (Out);
	    G_debug (3, "nlines =  %d\n", nlines );
	}
    }
    fprintf (stderr, "\n" ); 

    G_free ( XPnts );
    /* TODO: free Rtree */

    return 1;
}

