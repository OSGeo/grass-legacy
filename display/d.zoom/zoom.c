#include "gis.h"
#include "display.h"
#include "raster.h"
#include "local_proto.h"

int zoomwindow ( struct Cell_head *window, int quiet, double magnify)
{
    int screen_x, screen_y, button;
    int end = 0;
    int printmenu = 1;

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
		pan_window(window, screen_x, screen_y);
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
    return(0) ;
}
