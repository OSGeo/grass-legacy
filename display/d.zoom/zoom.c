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
	    fprintf(stderr, _("%s Zoom menu\n"), lefts);
	    fprintf(stderr, _("%s Pan\n"), middles);
	    fprintf(stderr, _("%s Quit menu\n"), rights);
	    printmenu = 0;
	}

	R_get_location_with_pointer(&screen_x, &screen_y, &button);

	if(button == leftb){
	    /* enter zoom menu */
	        make_window_box (window, magnify, 1, 0);
	        printmenu = 1;
	}else
	if(button == middleb){
	    /* pan */
		pan_window(window, screen_x, screen_y);
	}else
	if(button == rightb){
	      	end = 1;
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
