/*
 * $Id$
 */

/* possible TODO: add support for magnify (zoom=) */

#include "gis.h"
#include "display.h"
#include "raster.h"
#include "local_proto.h"
#include "glocale.h"


int do_pan(struct Cell_head *window) {
    int screen_x, screen_y, button;
    int end=0, printmenu=1;

    while( !end ) {
	if (printmenu){
	    fprintf(stderr, _("\n\nButtons:\n"));
	    fprintf(stderr, _("%s Pan\n"), LEFTS);
	    fprintf(stderr, _("%s Quit\n"), RIGHTS);
	    printmenu = 0;
	}

	R_get_location_with_pointer(&screen_x, &screen_y, &button);


	switch(button) {
	    case LEFTB: /* pan */
		pan_window(window, screen_x, screen_y);
		printmenu = 1;
		break;
	    case MIDDLEB: /* noop */
		printmenu = 1;
		break;
	    case RIGHTB:
		end = 1;
		break;
	}
    }

    return(0);
}



int pan_window (struct Cell_head *window, int screen_x, int screen_y)
{
    double px, py, uxc, uyc, ux1, uy1, ux2, uy2;
    double north,south,east,west, ns, ew;
    int t;

    px = D_d_to_u_col((double)screen_x);
    py = D_d_to_u_row((double)screen_y);
    fprintf(stderr, "\n");
    print_coor ( window, py, px );
    fprintf(stderr, "\n");

    uxc = D_d_to_u_col((double)screen_x);
    uyc = D_d_to_u_row((double)screen_y);
    t = uxc / window->ew_res;
    uxc = t * window->ew_res;
    t = uyc / window->ns_res;
    uyc = t * window->ns_res;

    ew = window->east - window->west;
    ns = window->north - window->south;

    ux1 = uxc - ew/2;
    ux2 = uxc + ew/2;
    uy1 = uyc - ns/2;
    uy2 = uyc + ns/2;

    north = uy1>uy2?uy1:uy2;
    south = uy1<uy2?uy1:uy2;
    west  = ux1<ux2?ux1:ux2;
    east  = ux1>ux2?ux1:ux2;

    if (  window->proj == PROJECTION_LL ) {
    	if ( north > 90 ) { 
    	    north =  90;
    	    south = 90 - ns;
    	} else if ( south < -90 ) {
    	    south =  -90;
    	    north = -90 + ns;
    	}
    }

    set_win (window, east, north, west, south, 0);

    return(0);
}

