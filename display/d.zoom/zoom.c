#include "gis.h"
#include "display.h"
#include "raster.h"
#include "local_proto.h"
#include "glocale.h"

int zoomwindow ( struct Cell_head *window, int quiet, double magnify)
{
    int screen_x, screen_y, button;
    int end = 0;
    int printmenu = 1;

    while( !end ) {
	if (printmenu){
	    fprintf(stderr, _("\n\nButtons:\n")) ;
	    fprintf(stderr, _("%s Zoom menu\n"), LEFTS);
	    fprintf(stderr, _("%s Pan\n"), MIDDLES);
	    fprintf(stderr, _("%s Quit\n"), RIGHTS);
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
	fprintf(stderr, _("This region now saved as current region.\n\n")) ;
	fprintf(stderr, _("Note: run 'd.erase' for the new region to affect the graphics.\n"));
    }
#endif
    return(0) ;
}
