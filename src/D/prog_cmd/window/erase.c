/*  %W%  %G%  */

/*
 *   Derase
 *
 *   Usage:  Derase color
 *
 *   Erase a window.  This erases the memory of what was on the window.
 */

#include <stdio.h>
#define USAGE	"[color]"

main(argc, argv)
	int argc ;
	char **argv ;
{
	char buff[128] ;
	char window_name[64] ;
	int color ;
	int i ;
	int t, b, r, l, line ;

/* Initialize the GIS calls */
	G_gisinit("Derase") ;

/* Check command line */
	if (argc > 2)
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}

/* Decipher color */
	if (argc == 2)
	{
		if (! (color = D_translate_color(argv[1])))
			color = D_translate_color("black") ;
	}
	else
	{
		color = D_translate_color("black") ;
	}

	R_open_driver();

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current window") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current window not available") ;

	Derase(color) ;

	R_close_driver();
	exit(0);
}
