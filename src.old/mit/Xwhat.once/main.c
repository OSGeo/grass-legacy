/*  %W%  %G%  */
/*
 *   Xwhat
 *
 *   Usage:  Xwhat [layer]
 *
 */

#define GLOBAL
#include "what.h"

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

Display* the_display;
Window the_window;
int the_screen;
XEvent report;
unsigned long event_mask;

main (argc, argv)
	int argc ;
	char **argv ;
{
	int width;
	int i;
	int row, col;
	int nrows, ncols;
	CELL *buf;
	struct Cell_head window;
	int screen_x, screen_y ;
	double east, north ;
	int button ;
	double XD_get_d_north(), XD_get_d_south() ;
	double XD_get_d_east(), XD_get_d_west() ;
	double XD_d_to_u_row(), XD_d_to_u_col() ;
	char temp[128];

	Window root_return;
        unsigned int x, y, window_width, window_height, 
		border_width, depth;

	/* Set the display to be the default display */
        if ((the_display = XOpenDisplay("")) == NULL)
        {
        printf(" can't open display\n");
        return(-1);
        }

	the_screen = DefaultScreen(the_display);

        the_window = XD_get_cur_window(the_display,
                                the_screen);

	/* Initialize the GIS calls */
        G_gisinit (argv[0]) ;

	/* Read in the map window associated with window */
        G_get_window(&window) ;

	if ((argc == 1) || (strcmp (argv[1], "-") == 0))
        {
	XD_get_cell_name (the_display, the_window, temp);

	if (*temp == 0)
          {
          fprintf (stderr, "warning: no data layer drawn in current window\n");
	  exit(-1) ;
          }
        else if ((fd = opencell (temp, name, mapset)) < 0)
          {
          fprintf (stderr, "warning: inappropriate data layer in current window\n");
          exit(-1) ;
          } 
        }
        else if ((fd = opencell (argv[1], name, mapset)) < 0)
        {
          fprintf (stderr, "warning: data layer [%s] not found\n", argv[1]);
          exit(-1) ;
        }

	event_mask = ButtonPressMask;

        XSelectInput(the_display, the_window,
				event_mask);

	XGetGeometry(the_display, the_window,
                &root_return, &x, &y, &window_width,
                &window_height, &border_width, &depth);

	if (XD_do_conversions(&window,
                window_width, window_height))
        G_fatal_error("Error in calculating conversions");

	nrows = window.rows;
	ncols = window.cols;
	buf = G_allocate_cell_buf();

	XMaskEvent(the_display, event_mask, &report);

	screen_x = ((XButtonPressedEvent *)&report)->x;
        screen_y = ((XButtonPressedEvent *)&report)->y;
	button = ((XButtonPressedEvent *)&report)->button;

	east  = XD_d_to_u_col((double)screen_x) ;
	north = XD_d_to_u_row((double)screen_y) ;

	
	row = (window.north - north) / window.ns_res ;
	col = (east - window.west) / window.ew_res ;


	if (row < 0 || row >= nrows || col < 0 || col >= ncols)
	{
		printf("0 3\n") ;
		return ;
	}

	if (G_get_map_row (fd, buf, row) < 0)
		printf("0 3\n") ;
	else
		printf("%d %d\n", buf[col], button) ;
}
