#include "gis.h"
#include "display.h"
#include "raster.h"
#include "local_proto.h"

int zoomwindow ( struct Cell_head *window, int quiet, double magnify)
{
    struct Cell_head oldwindow, defwin;
    char *err;
    int quitonly;
    int screen_x, screen_y, button;
    int end = 0;
    double px, py, uxc, uyc, ux1, uy1, ux2, uy2;
    double north,south,east,west, ns, ew;
    int printmenu = 1;
    int t, limit;

    G_copy((char *) &oldwindow, (char *) &window, sizeof(window));
    G_get_default_window(&defwin);

    while( !end ) {
	if (printmenu){
	    fprintf(stderr, "\n\nButtons:\n") ;
	    fprintf(stderr, "%s Zoom menu\n", LBTN);
	    fprintf(stderr, "%s Pan\n", MBTN);
	    fprintf(stderr, "%s Quit\n", RBTN);
	    printmenu = 0;
	}

	R_get_location_with_pointer(&screen_x, &screen_y, &button);

	
	switch(button) {
	    case LEFTB: /* enter zoom menu */
	        make_window_box (window, magnify, 1, 0);
	        printmenu = 1;
		break ;
	    case MIDDLEB: /* pan */
		/* For print only */
		px = D_d_to_u_col((double)screen_x)  ;
		py = D_d_to_u_row((double)screen_y)  ;
	        fprintf(stderr, "\n") ;
		print_coor ( window, py, px );
	        fprintf(stderr, "\n") ;
		
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
                uy2 = uyc + ns/2;\

		north = uy1>uy2?uy1:uy2 ;
		south = uy1<uy2?uy1:uy2 ;
		west  = ux1<ux2?ux1:ux2 ;
		east  = ux1>ux2?ux1:ux2 ;

		if (  window->proj == PROJECTION_LL ) {
		    if ( north > 90 ) { 
			north =  90;
			south = 90 - ns;
		    } else if ( south < -90 ) {
			south =  -90;
			north = -90 + ns;
		    }
		}
		

		set_win ( window, east, north, west, south, 0);
		break ;
            case RIGHTB:
	      	end = 1;
		break ;
        }		
    }

#ifdef QUIET
    if(!quiet)
    {
	fprintf(stderr, "This region now saved as current region.\n\n") ;
	fprintf(stderr, "Note: run 'd.erase' for the new region to affect the graphics.\n");
    }
#endif
    return(quitonly) ;
}

