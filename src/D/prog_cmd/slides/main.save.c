/*  %W%  %G%  */

/*
 */

#include <stdio.h>
#define MAIN
#define USAGE	" filename"

main(argc, argv)
	int argc ;
	char **argv ;
{
	int t, b, l, r ;

/* Initialize the GIS calls */
	G_gisinit("Dsave.slide") ;

/* Check command line */
	if ( argc != 2)
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}

/* */
	R_open_driver();


/* Get the screen dimensions */
	t = R_screen_top() ;
	b = R_screen_bot() ;
	l = R_screen_left() ;
	r = R_screen_rite() ;

	R_panel_save( argv[1], t, b, l, r) ;

	R_close_driver();
}
