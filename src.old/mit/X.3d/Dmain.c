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
	char *location        ;
	char *mapset         ;
	char *name[20]        ;
	char buffer[256]      ;
	extern int stash_away() ;
	char window_name[64] ;
	int do_it ;

	G_gisinit("argv[0]") ;

	G_get_window(&window) ;

/* Gather the 3d display parameters */
	set_default_options(&window) ;

	if (D_parse_command(argc, argv, variables, n_variables, stash_away))
	{
		usage(argv[0]) ;
		exit(-1) ;
	}

	if (show_default)
	{
		print_defaults() ;
		exit(0) ;
	}

	if (check_options() )
		G_fatal_error("Inappropriate 3d request") ;
	
	if (0 > G_set_window(&window) )
		G_fatal_error("Inappropriate window resolution request") ;

	if((the_display=XOpenDisplay("")) == NULL)
	{
	G_fatal_error(" Can't open display\n");
	}

	the_screen = DefaultScreen(the_display);

	the_window = XD_get_cur_window(the_display, the_screen);

	the_gc=XCreateGC(the_display,the_window,None, &the_gc_values);

	if (0 > G_set_window(&window) )
	G_fatal_error("Inappropriate window resolution request") ;

	establish_view(from_easting,from_northing,from_height,
		to_easting,to_northing,to_height,field) ;

	threed(1);

}

print_defaults()
{
	printf("file=[MAPNAME(%s)]", file) ;
	printf(" fe=[FROM_EASTING(%.2lf)]", from_easting) ;
	printf(" fn=[FROM_NORTHING(%.2lf)]", from_northing) ;
	printf(" fh=[FROM_HEIGHT(%.2lf)]", from_height) ;
	printf(" te=[TO_EASTING(%.2lf)]", to_easting) ;
	printf(" tn=[TO_NORTHING(%.2lf)]", to_northing) ;
	printf(" th=[TO_HEIGHT(%.2lf)]", to_height) ;
	printf(" ew=[EW_RES(%.2lf)]", window.ew_res) ;
	printf(" ns=[NS_RES(%.2lf)]", window.ns_res) ;
	printf(" lf=[LINE_FREQ(%d)]", line_freq) ;
	printf(" exag=[EXAG(%.2lf)]", exag) ;
	printf(" ef=[ELEVFILE(%s)]", elevfile) ;
	printf(" va=[VIEWANGLE(%.2lf)]", field) ;
	printf("\n") ;
}
