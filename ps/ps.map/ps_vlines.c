/* Functions: PS_vector_plot
**
** Modified by: Janne Soimasuo August 1994 line_cat added
** Author: Paul W. Carlson	March 1992
** modified to use G_plot_line() by Olga Waupotitsch on dec,93
*/

#include <grass/Vect.h>
#include "ps_info.h"
#include "local_proto.h"
#include "vector.h"

int PS_vlines_plot (struct Map_info *P_map, int vec, int type)
{   
    struct line_pnts *Points, *nPoints, *pPoints;
    int i, k, np, line, cat, nlines, ret;
    double *xarray, *yarray, tol=0.1, width;
    struct line_cats *Cats;
    VARRAY *Varray = NULL;

    fprintf(PS.fp, "1 setlinejoin\n"); /* set line join to round */

    /* Create vector array if required */
    if ( vector.layer[vec].cats != NULL || vector.layer[vec].where != NULL ) {
	Varray = Vect_new_varray ( Vect_get_num_lines(P_map) );
        if ( vector.layer[vec].cats != NULL ) {
	    ret = Vect_set_varray_from_cat_string (P_map, vector.layer[vec].field,
	                 vector.layer[vec].cats, vector.layer[vec].ltype, 1, Varray );
	} else {
	    ret = Vect_set_varray_from_db (P_map, vector.layer[vec].field,
	                 vector.layer[vec].where, vector.layer[vec].ltype, 1, Varray );
	}
	G_debug ( 3, "%d items selected for vector %d", ret, vec );
    }

    /* allocate memory for coordinates */
    Points = Vect_new_line_struct();
    nPoints = Vect_new_line_struct();
    Cats = Vect_new_cats_struct ();

    /* process only vectors in current window */
    Vect_set_constraint_region(P_map, PS.w.north, PS.w.south, PS.w.east, PS.w.west,
	                       PORT_DOUBLE_MAX, -PORT_DOUBLE_MAX);

    tol /= PS.ew_to_x ; /* tolerance for parallel map units */
    width = vector.layer[vec].width;

    /* read and plot vectors */
    k = 0;
    nlines = Vect_get_num_lines ( P_map );
    for ( line = 1; line <= nlines; line++ ) {
	int ret;
	double d;
	if (0 > (ret = Vect_read_line(P_map, Points, Cats, line))) {
	    if (ret == -1) G_warning("Read error in vector file\n");
	    break;
	}
	if ( !(ret & GV_LINES) ) continue;
	if ( !(ret & vector.layer[vec].ltype) ) continue;

        if ( Varray != NULL && Varray->c[line] == 0 ) continue; /* is not in array */
	
	pPoints = Points;
	Vect_cat_get( Cats, 1, &cat);

	if(vector.layer[vec].cwidth) {
	    if(cat == 0 ) /* don't draw zero width line */     
		continue;
	
	    if(type == LINE_DRAW_HIGHLITE ) 
		width = cat * vector.layer[vec].cwidth + 2. * vector.layer[vec].hwidth;   

	    if(type == LINE_DRAW_LINE)
		width = cat * vector.layer[vec].cwidth;   

	    fprintf(PS.fp, "%.8f W\n", width);   
	}

	if ( vector.layer[vec].coffset != 0 || vector.layer[vec].offset != 0 ) {
	    if ( vector.layer[vec].coffset != 0 )
		d = cat * vector.layer[vec].coffset / PS.ew_to_x ;
	    else
		d = vector.layer[vec].offset / PS.ew_to_x ;
	    
	    adjust_line ( Points ); /* LL projection */
	    Vect_line_parallel ( Points, d, tol, 1, nPoints );
	    pPoints = nPoints;
	}
	
	if ( vector.layer[vec].ref == LINE_REF_CENTER ) {
	    np = pPoints->n_points;
	    xarray = pPoints->x;
	    yarray = pPoints->y;

	    if ( pPoints->n_points > 1) {
		start_line(xarray[0], yarray[0]);

		for (i = 0; i < np - 1; i++) {
		    sec_draw = 0;
		    G_plot_line(xarray[0], yarray[0], xarray[1], yarray[1]);
		    if (k == 2) {
			fprintf(PS.fp, "\n");
			k = 0;
		    } else {
			fprintf(PS.fp, " ");
			k++;
		    }
		    xarray++;
		    yarray++;
		}
		fprintf(PS.fp, "D\n");
	    }
	} else {
	    /* draw line as filled polygon between original line and parallel line (offset=width) */
	    d = width / PS.ew_to_x; 
	    if ( vector.layer[vec].ref == LINE_REF_RIGHT ) d =- d;
	    adjust_line ( Points ); /* LL projection */
	    Vect_line_parallel ( Points, d, tol, 1, nPoints );
	    Vect_line_reverse ( nPoints );

	    fprintf(PS.fp, "NP\n");
	    if ( Points->n_points > 0 ) {
		construct_path (Points, 0, START_PATH);
		construct_path (nPoints, 0, CLOSE_PATH);
	    } else {
		construct_path (Points, 0, WHOLE_PATH);
	    }
	    fprintf(PS.fp, "F\n");
	}
	Vect_reset_line ( Points );
    }
    fprintf(PS.fp, "\n");
    fprintf(PS.fp, "0 setlinejoin\n"); /* reset line join to miter */
    return 0;
}

