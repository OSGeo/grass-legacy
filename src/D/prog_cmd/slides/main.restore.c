/*  %W%  %G%  */

/*
 */

#include <stdio.h>
#define MAIN

main(argc, argv)
	int argc ;
	char **argv ;
{

/* Initialize the GIS calls */
	G_gisinit("Drestore.slide") ;

/* Check command line */
	if ( argc != 2)
	{
		fprintf(stderr,"Usage: %s filename\n", argv[0]) ;
		fprintf(stderr,"Must be a file from Dsave.slide\n") ;
		exit(-1) ;
	}

/* */
	R_open_driver();

	R_panel_restore(argv[1]) ;

	R_close_driver();
}
