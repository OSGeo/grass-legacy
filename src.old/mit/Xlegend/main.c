/*  %W%  %G%  */

/*
 *   Xlegend
 *
 *   Usage:  Xlegend mapname [color]
 *           Xlegend name=mapname color=name type=type lines=nlines
 *
 *   Print a legend for a map 
 */

#define USAGE	"name=mapname [color=name]"
#include "gis.h"
#define MAIN
#include "options.h"
#include "driver.h"

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

Display *the_display;
Window the_window;
Colormap colormap;
int the_screen;
GC the_gc;
XGCValues values;

main(argc, argv)
	int argc ;
	char **argv ;
{
	char buff[256] ;
	char window_name[64] ;
	char *mapset ;
	struct Categories cats ;
	struct Range range ;
	struct Colors colors ;
	unsigned long white ;
	unsigned long black ;
	int cats_num ;
	int cur_dot_row ;
	int dots_per_line ;
	int dots_per_point ;
	int i ;
	int do_cats ;
	XPoint pts[5];
	extern int stash_away() ;

	XWindowAttributes win_atts;
	unsigned int window_width, window_height,
                border_width, depth ;
        int x, y;
        Window root_return;

/* Initialize the GIS calls */
	G_gisinit("Xlegend") ;

/* Check command line */
	set_default_options() ;

	if (D_parse_command(argc, argv, variables, n_variables, stash_away))
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}

	if (! strlen(map_name))
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}

/* Make sure map is available */
	mapset = G_find_cell (map_name, "") ;
	if (mapset == NULL)
	{
		sprintf(buff,"Cellfile [%s] not available", map_name);
		G_fatal_error(buff) ;
	}

	/* Set the display to be the default display */
        if ((the_display = XOpenDisplay("")) == NULL)
        {
        printf(" can't open display\n");
        return(-1);
        }

        the_screen = DefaultScreen(the_display);

        the_window = XD_get_cur_window(the_display,
                        the_screen);


        the_gc = XCreateGC(the_display, the_window,
                        0, None);

        XGetGeometry(the_display, the_window,
                &root_return, &x, &y, &window_width,
                &window_height, &border_width, &depth);


        XGetWindowAttributes(the_display, the_window,
                                        &win_atts);

        colormap = win_atts.colormap;

	init_font("romand");


	if (G_read_colors(map_name, mapset, &colors) == -1)
	{
		sprintf(buff,"color file for [%s] not available", map_name) ;
			G_fatal_error(buff) ;
	}

	if (G_read_cats(map_name, mapset, &cats) == -1)
	{
		sprintf(buff,"Category file for [%s] not available", map_name) ;
			G_fatal_error(buff) ;
	}

	if (G_read_range(map_name, mapset, &range) == -1)
	{
		sprintf(buff,"Range information for [%s] not available", map_name) ;
			G_fatal_error(buff) ;
	}


/*
	if (D_get_cur_wind(window_name))
		G_fatal_error("No current window") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current window not available") ;
*/

	/* D_reset_colors(&colors); */

	white = WhitePixel(the_display, the_screen) ;
	black = BlackPixel(the_display, the_screen) ;


/* How many categories to show */
	cats_num = range.pmax - range.nmin + 1 ;
	do_cats = (cats_num + 1) < lines ? cats_num : lines -1 ;

/* Figure number of lines, number of pixles per line and text size */
	if (lines == 0) lines = cats_num + 1 ;
	dots_per_line = window_height / lines ;

	Text_size((int)(dots_per_line*4/5), (int)(dots_per_line*4/5)) ;

/* Set up box arrays */
	pts[0].x = 4;
	pts[1].x = 0                 ;
	pts[1].y = (short) (6-dots_per_line) ;
	pts[2].x = (short) (dots_per_line-6) ;
	pts[2].y = 0                 ;
	pts[3].x = 0                 ;
	pts[3].y = (short) (dots_per_line-6) ;
	pts[4].x = (short) (6-dots_per_line) ;
	pts[4].y = 0                 ;

/* Draw away */
	cur_dot_row =  dots_per_line/2;
	for(i=range.nmax; i<=range.pmax; i++)
	{
	/* White box */
		XSetForeground(the_display, the_gc, white);
		cur_dot_row += dots_per_line;
		Move_abs(2, (cur_dot_row-1)) ;
		Cont_rel(0, (2-dots_per_line)) ;
		Cont_rel((dots_per_line-2), 0) ;
		Cont_rel(0, (dots_per_line-2)) ;
		Cont_rel((2-dots_per_line), 0) ;

	/* Black box */
		XSetForeground(the_display, the_gc, black);
		Move_abs(3, (cur_dot_row-2)) ;
		Cont_rel(0, (4-dots_per_line)) ;
		Cont_rel((dots_per_line-4), 0) ;
		Cont_rel(0, (dots_per_line-4)) ;
		Cont_rel((4-dots_per_line), 0) ;

	/* Color solid box */
		/*
		R_color(i) ;
		Move_abs(l+4, (cur_dot_row-3)) ;
		R_polygon_rel(x_box, y_box, 5) ;
			*/

		pts[0].y = (short) (cur_dot_row-3);

		XSetForeground(the_display, the_gc,
				(unsigned long) i);

		XFillPolygon(the_display, the_window,
			the_gc, &pts[0], 5, Convex, 
			CoordModePrevious); 
			

	/* Draw text */
		XSetForeground(the_display, the_gc,
		XD_make_colr(the_display, the_window,
				the_screen,
				colormap, color));
		sprintf(buff, "%2d) %s", i, G_get_cat(i, &cats)) ;
		Move_abs((3+dots_per_line), (cur_dot_row)) ;
		Text(buff) ;
	}

	XFlush(the_display);

/* Add this command to list */
/*
	strcpy(buff, argv[0]) ;
	for(i=1; i<argc; i++)
	{
		strcat(buff, " ") ;
		strcat(buff, argv[i]) ;
	}
	D_add_to_list(buff) ;
*/

}
