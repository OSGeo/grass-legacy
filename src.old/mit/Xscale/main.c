/*  %W%  %G%  */

/*   Xscale
 *
 *   Usage:  Xscale color1 color2
 *           Xscale color1=name color2=name
 *
 *   Draw an "appropriate" scale on the map
 */

#include "gis.h"
#define MAIN
#include "options.h"
#include "driver.h"

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>


#define NUMSCALES	10
#define USAGE   "[color1=name] [color2=name]"

Display *the_display;
Window the_window;
Colormap colormap;
int the_screen;
GC the_gc;
XGCValues values;


/* declare variables */
static struct 
	{
	char *name ;
	double size ;
	double limit ;
	} scales[] =
		{ ""          ,      0.,     20.,
		  "10 meters" ,     10.,     70.,
		  "50 meters" ,     50.,    200.,
		  "100 meters",    100.,    700.,
		  "500 meters",    500.,   2000.,
		  "1 km"      ,   1000.,   7000.,
		  "5 km"      ,   5000.,  20000.,
		  "10 km"     ,  10000.,  70000.,
		  "50 km"     ,  50000., 200000.,
		  "100 km"    , 100000., 700000.
		} ;

struct Cell_head window;


main(argc, argv)
        int argc ;
        char **argv ;
{
        char buff[128] ;
        char window_name[64] ;
        int i ;
        extern int stash_away() ;

/* Initialize the GIS calls */
        G_gisinit("Xscale") ;

/* Check command line */
        set_default_options() ;

        if (D_parse_command(argc, argv, variables, 
		n_variables, stash_away))
        {
                fprintf(stderr,"Usage: %s %s\n", argv[0], 
			 USAGE);
                exit(-1) ;
        }




/*
        if (D_get_cur_wind(window_name))
                G_fatal_error("No current window") ;

        if (D_set_cur_wind(window_name))
                G_fatal_error("Current window not available"
) ;
*/

/* Read in the map window associated with window */
        G_get_window(&window) ;

/*
        if (D_check_map_window(&window))
                G_fatal_error("Setting map window") ;

        if (G_set_window(&window) == -1)
                G_fatal_error("Current window not settable")
 ;
*/


/* Draw the scale */
        draw_scale(color1, color2) ;

	XFlush(the_display);

/* Add this command to list 
        strcpy(buff, argv[0]) ;
        for(i=1; i<argc; i++)
        {
                strcat(buff, " ") ;
                strcat(buff, argv[i]) ;
        }
        D_add_to_list(buff) ;
*/

}



draw_scale (color1, color2)
	char *color1, *color2;
{
	unsigned long colr1, colr2;
	double meters ;
	int line_len ;
	int incr ;
	double XD_get_a_to_d_xconv() ;
	double XD_get_u_east(), XD_get_u_west() ;
	double XD_get_d_west(), XD_get_d_north() ;
	int D_west, D_north ;
	double XD_get_ew_resolution() ;
	int t, b, l, r ;
	int i ;
	int size ;

	XWindowAttributes win_atts;
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


        the_gc = XCreateGC(the_display, the_window,
                        0, None);

        XGetGeometry(the_display, the_window,
                &root_return, &x, &y, &window_width,
                &window_height, &border_width, &depth);

	if (XD_do_conversions(&window,
                window_width, window_height))
    G_fatal_error("Error in calculating conversions");

        XGetWindowAttributes(the_display, the_window,
                                        &win_atts);

        colormap = win_atts.colormap;

	colr1 = XD_make_colr(the_display, 
			the_window, the_screen,
			colormap, color1);

	colr2 = XD_make_colr(the_display, 
			the_window, the_screen,
			colormap, color2);

	init_font("romand");

/* Establish text size */
	size = (int)(.025 * (float) window_height) ;
	Text_size(size, size) ;

	meters  = XD_get_u_east() - XD_get_u_west() ;
	D_north  = (int)XD_get_d_north() ;
	D_west   = (int)XD_get_d_west() ;

/* find the right scale */
	for (incr=0; incr<NUMSCALES; incr++)
	{
		if (meters <= scales[incr].limit)
			break ;
	}

	if (! incr)
		return(-1) ;

	line_len = (int)(XD_get_a_to_d_xconv() * (scales[incr].size / XD_get_ew_resolution()) ) ;

/* Blank out area with background color */
	XSetForeground(the_display, the_gc, colr1);
	r = D_west + 35 + line_len + size * strlen(scales[incr].name) ;
	for(i=D_north + 5; i < D_north + 35; i++)
		Move_abs(D_west+5, i), Cont_abs(r, i) ;
		
/* Draw legend */
	XSetForeground(the_display, the_gc, colr2);
	Move_abs (D_west + 10, D_north + 25) ;
	Cont_rel ( 0,-10) ;
	Cont_rel (10, 10) ;
	Cont_rel ( 0,-10) ;
	Move_rel (-5, 14) ;
	Cont_rel ( 0,-17) ;
	Cont_rel (-2, -0) ;
	Cont_rel ( 2, -2) ;
	Cont_rel ( 2,  2) ;
	Cont_rel (-2, -0) ;
	Move_abs (D_west + 30, D_north + 10) ;
	Cont_abs (D_west + 30, D_north + 30) ;
	Move_abs (D_west + 30, D_north + 20) ;
	Cont_abs (D_west + 30 + line_len, D_north + 20) ;
	Move_abs (D_west + 30 + line_len, D_north + 10) ;
	Cont_abs (D_west + 30 + line_len, D_north + 30) ;

	Move_abs (D_west + 40 + line_len, D_north + 25) ;
	Text(scales[incr].name) ;

	

	return(0) ;
}
