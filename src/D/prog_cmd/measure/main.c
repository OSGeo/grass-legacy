/*  %W%  %G%  */

/*
 *   Dmeasure
 *
 *   Usage:  Dmeasure [forecolor] [backcolor]
 *
 *   Linear and area measure
 */

#include <stdio.h>
#define USAGE	"[forecolor=name] [backcolor=name]"

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
	G_gisinit("Dmeasure") ;

/* Check command line */
	if (argc > 3)
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

	measurements() ;

	R_close_driver();
}
