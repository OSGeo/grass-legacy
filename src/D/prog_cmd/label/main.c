/*  %W%  %G%  */

/*
 *   Dlabel
 *
 *   Usage:  Dlabel [textcolor] [backcolor] [size] [font]
 *           Dlabel [textcolor=name] [backcolor=name] [size=num] [font=name]
 *
 *   Draw interactive labels in a text window.
 */

#include <stdio.h>
#define MAIN
#include "options.h"
#define USAGE	"[textcolor=name] [backcolor=name] [size=num] [font=name]"

main(argc, argv)
	int argc ;
	char **argv ;
{
	char window_name[64] ;
	int i ;
	int t, b, l, r ;
	extern int stash_away() ;
	int tsize ;

/* Initialize the GIS calls */
	G_gisinit("Dlabel") ;

/* Check command line */
	set_default_options() ;

	if (D_parse_command(argc, argv, variables, n_variables, stash_away))
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}

/* */
	R_open_driver();

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current window") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current window not available") ;

/* Figure out where to put text */
	D_get_screen_window(&t, &b, &l, &r) ;
	R_set_window(t, b, l, r) ;

	dots_per_line = (int)(size/100.0 * (float)(b - t)) ;
	tsize = (int)(.8 * (float)dots_per_line) ;
	R_text_size(tsize, tsize) ;

	label(t, b, l, r) ;

	R_close_driver();
}
