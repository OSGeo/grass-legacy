/* %W%  %G%  */
#include "gis.h"
#define MAIN
#include "options.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>

Window the_window;
Display* the_display;
GC the_gc;
XGCValues the_gc_values;
int the_screen;

/* 3d program gathers input for D3d which is then executed */

main(argc, argv)
	int argc;
	char *argv[];
{
	int do_it, do_erase ;

	G_gisinit("3D") ;

	G_get_window(&window) ;

	if((the_display=XOpenDisplay("")) == NULL)
	{
	G_fatal_error(" Can't open display\n");
	}

	the_screen = DefaultScreen(the_display);

	the_window = XD_get_cur_window(the_display,
				the_screen);


	the_gc=XCreateGC(the_display,the_window,None,
					    &the_gc_values);

	for(;;)
	{
	get_inputs(&do_it, &do_erase) ;
	if(! do_it)
		break;

	if (check_options() )
		G_fatal_error("Inappropriate 3d request") ;

	printf("\n yoyo");

	if (0 > G_set_window(&window) )
	G_fatal_error("Inappropriate window resolution request") ;

	if(do_erase)
	XClearWindow(the_display, the_window);

	establish_view(from_easting,from_northing,from_height,
		to_easting,to_northing,to_height,field) ;

		threed(1);

	}	/* end of for	*/	
}
