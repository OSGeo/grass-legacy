#include "gis.h"
#include "Vect.h"
#include "dbmi.h"
#include "display.h"
#include "raster.h"
#include "symbol.h"
#include "global.h"

int 
bar ( double cx, double cy, int size, double scale, double *val, int ncols, COLOR *ocolor, COLOR *colors ) 
{
    int i, j;
    double max;
    double x0, y0;
    double bw; /* bar width */
    double pixel; /* pixel size */
    struct line_pnts *Points;

    G_debug (4, "bar(): cx = %f cy = %f", cx, cy );
    
    Points = Vect_new_line_struct ();

    pixel = D_d_to_u_col(2) - D_d_to_u_col(1); /* do it better */
    
    /* Bottom (y0) */
    max = 0;
    for ( i = 0; i < ncols; i++ ) { 
	if ( val[i] > max) max = val[i]; 
    };
    y0 = cy - scale * max * pixel / 2;
    
    /* Left (x0) */
    x0 = cx - size * pixel / 2;

    bw = size * pixel / ncols;
	
    /* Draw polygon for each value */
    for ( i = 0; i < ncols; i++ ) {
	Vect_reset_line ( Points );
	Vect_append_point ( Points, x0 + i*bw, y0, 0);
	Vect_append_point ( Points, x0 + (i+1)*bw, y0, 0);
	Vect_append_point ( Points, x0 + (i+1)*bw, y0 + scale * val[i] * pixel, 0);
	Vect_append_point ( Points, x0 + i*bw, y0 + scale * val[i] * pixel, 0);
	Vect_append_point ( Points, x0 + i*bw, y0, 0);

	if(!colors[i].none) {
	    R_RGB_color ( colors[i].r, colors[i].g, colors[i].b );
	    G_plot_polygon ( Points->x, Points->y, Points->n_points);
	}

	R_RGB_color ( ocolor->r, ocolor->g, ocolor->b );
	for (j = 1; j < Points->n_points; j++ ) { 
	    G_plot_line(Points->x[j], Points->y[j], Points->x[j-1], Points->y[j-1]);
	}
    }

    Vect_destroy_line_struct (Points);
    
    return 0;
}


