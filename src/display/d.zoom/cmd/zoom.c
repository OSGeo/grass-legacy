#include "gis.h"
#include "display.h"
#include "raster.h"
#include "local_proto.h"

int zoomwindow (int quiet, double magnify)
{
    struct Cell_head window, oldwindow, defwin;
    char *err;
    int quitonly;
    int screen_x, screen_y, button;
    int end = 0;
    double px, py, uxc, uyc, ux1, uy1, ux2, uy2;
    double north,south,east,west, ns, ew;
    int printmenu = 1;
    int limit;

    G_get_set_window(&window);
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
	        make_window_box (&window, magnify);
	        printmenu = 1;
		break ;
	    case MIDDLEB: /* pan */
		/* For print only */
		px = D_d_to_u_col((double)screen_x)  ;
		py = D_d_to_u_row((double)screen_y)  ;
	        fprintf(stderr, "\n") ;
		print_coor ( &window, py, px );
	        fprintf(stderr, "\n") ;
		
		uxc = D_d_to_u_col((double)screen_x);
		uyc = D_d_to_u_row((double)screen_y);
		ew = window.east - window.west;
		ns = window.north - window.south;
		ux1 = uxc - ew/2;
                ux2 = uxc + ew/2;
                uy1 = uyc - ns/2;
                uy2 = uyc + ns/2;

		limit = 0;
		if ( uy2 > defwin.north ) {
		    north = defwin.north;
		    south = defwin.north - ns;
	            fprintf(stderr, "\nNorth limit of region reached") ;
		    limit = 1;
		} else if ( uy1 < defwin.south ) {
		    /*
		    south = defwin.south;
		    north = defwin.south + ns;
		    */
	            fprintf(stderr, "\nSouth limit of region reached") ;
		    limit = 1;
		} else { 
		    north = uy2;
		    south = uy1;
		}
		north = uy2;
		south = uy1;
		if ( ux1 < defwin.west ) {
		    west  = defwin.west;
		    east  = defwin.west + ew;
	            fprintf(stderr, "\nWest limit of region reached") ;
		    limit = 1;
		} else if ( ux2 > defwin.east ) {
		    east  = defwin.east;
		    west  = defwin.east - ew ;
	            fprintf(stderr, "\nEast limit of region reached") ;
		    limit = 1;
		} else {
		    west  = ux1;
		    east  = ux2;
		}
                if ( limit ){
		    printmenu = 1;
	            fprintf(stderr, "\n\n") ;
		}
		west  = ux1;
		east  = ux2;
		
		window.north = north;
		window.south = south;
		window.east  = east ;
		window.west  = west ;

                print_win ( &window, north, south, east, west );
		printmenu = 1;
		
		G_put_window(&window);
	        G_set_window(&window);
		redraw();
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

