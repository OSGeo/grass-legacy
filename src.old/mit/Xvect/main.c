/* @(#)main.c	2.2   7/24/87 */

/*
 *   Xvect
 *
 *   Usage:  Xvect mapname [color]
 *           Xvect name=mapname color=name
 *
 *   Draw the binary vector (dig) file that
 *   the user wants displayed on top of the current image.
 */

#define USAGE	"name=mapname [color=name]"
#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "gis.h"
#include "digit.h"
#define MAIN
#include "options.h"

Display *the_display;
Window the_window;
int the_screen;
GC the_gc;
XGCValues the_gc_values;
unsigned long the_gc_value_mask;
Colormap colormap;

struct Cell_head window;

main(argc, argv)
	int argc ;
	char **argv ;
{
	FILE *fd ;
	char *mapset ;
	char buff[128] ;
	int i ;
	unsigned long foreground;
	unsigned long XD_make_colr();
	extern int stash_away() ;
	XWindowAttributes win_atts;
	Atom vect_name;

/* Initialize the GIS calls */
	G_gisinit("display vector map") ;

/* Check command line */
	set_default_options() ;

        if (D_parse_command(argc, argv, variables, 
		n_variables, stash_away) ||
            			*map_name == 0)
        {
        fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE);
        exit(-1) ;
        }

/* Make sure map is available */
	mapset = G_find_file ("dig", map_name, "") ;
	if (mapset == NULL)
	{
	sprintf(buff,"Vector file [%s] not available", map_name);
		G_fatal_error(buff) ;
	}


/* Check for and read dig header info */

	if (! (fd = G_fopen_old("dig", map_name, mapset) ) )
	{
	sprintf(buff, "Can't open vector file [%s] in [%s]",
			map_name, mapset) ;
		G_fatal_error(buff) ;
	}

	if (dig_init(fd) == -1)
	{
	fclose(fd) ;
	G_fatal_error("Problem in initial read of digit file") ;
	}

	/* Set the display to be the default display */
	if ((the_display = XOpenDisplay("")) == NULL)
	{
	printf("in %s: can't open display\n",argv[0]);
	return(-1);
	}

	the_screen = DefaultScreen(the_display);

	the_window = XD_get_cur_window(the_display,
				the_screen);

	G_get_window(&window);

	XGetWindowAttributes(the_display, the_window,
		&win_atts);

	colormap = win_atts.colormap;

	/* Decipher color */
        if (argc == 3)
        {
        foreground = XD_make_colr(the_display, 
			the_window, the_screen,
			colormap, color) ;
        }

	the_gc_values.foreground =  foreground ;

	the_gc = XCreateGC(the_display,the_window,
                   	GCForeground , 
			&the_gc_values);


	dig_print_header() ;


/* Do the plotting */
	if (dig_plot_all_lines(fd) == -1)
	{
	fclose(fd) ;
	G_fatal_error("Problem in reading of vector file") ;
	}

	fclose(fd) ;

	vect_name = XInternAtom(the_display, "vect", True);

        XChangeProperty(the_display, the_window,vect_name,
                XA_STRING, 8, PropModeReplace,
                map_name, 32);
	
	

	XFlush(the_display);

	XFreeGC(the_display, the_gc);

}

debugf()
{

	/*  null function while 'digit' is being debugged  */
}
