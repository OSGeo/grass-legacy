#include <stdlib.h>
#include <math.h>
#include "gis.h"

int trans ( double *x, double *y, int n_points, double angle, double scale, 
                     double xc, double yc ) {
    int i;
    double r, a;

    for ( i = 0; i < n_points; i++) {
	r = scale * hypot ( x[i], y[i] );
	a = atan2 ( y[i], x[i] );
	a += angle;   
	x[i] = r * cos(a) + xc;
	y[i] = r * sin(a) + yc;
    }

    return 1;
}

/* Plot icon */
int G_plot_icon (double xc, double yc, int type, double angle, double scale) 
{
    int    i, np = 0;
    double x[10], y[10];
    
    
    /* diamond, box */
    switch(type) {
	case G_ICON_CROSS: 
	    x[0] = -0.5; y[0] =  0.0;
	    x[1] =  0.5; y[1] =  0.0;
	    x[2] =  0.0; y[2] = -0.5;
	    x[3] =  0.0; y[3] =  0.5;
	    np = 4;
	    break;
	case G_ICON_BOX: 
	     G_debug (1, "box");
	    x[0] = -0.5; y[0] = -0.5;
	    x[1] =  0.5; y[1] = -0.5;
	    x[2] =  0.5; y[2] = -0.5;
	    x[3] =  0.5; y[3] =  0.5;
	    x[4] =  0.5; y[4] =  0.5;
	    x[5] = -0.5; y[5] =  0.5;
	    x[6] = -0.5; y[6] =  0.5;
	    x[7] = -0.5; y[7] = -0.5;
	    np = 8;
	    break;
	case G_ICON_ARROW: 
	    x[0] = -1; y[0] =  0.5;
	    x[1] =  0; y[1] =  0.0;
	    x[2] = -1; y[2] = -0.5;
	    x[3] =  0; y[3] =  0.0;
	    np = 4;
	    break;
    }
    
    trans ( x, y, np, angle, scale, xc, yc);
	    
    for ( i = 0; i < np; i += 2 )
        G_plot_line(x[i], y[i], x[i+1], y[i+1]);
		
    return (1);
}

