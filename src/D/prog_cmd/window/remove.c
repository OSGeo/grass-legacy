/*  %W%  %G%  */

/*
 *   Dremove
 *
 *   Usage:  Dremove
 *
 *   color a window black, then remove any trace of it.
 */

#include <stdio.h>

main(argc, argv)
	int argc ;
	char **argv ;
{
	char buff[128] ;
	char cur_wind[64] ;
	char new_wind[64] ;
	int color ;
	int i ;
	int t, b, r, l, line ;
	int button ;

/* Initialize the GIS calls */
	G_gisinit("Dremove") ;

/* Check command line */
	if (argc > 2)
	{
		D_usage(argv[0], "name") ;
		exit(1);
	}

	R_open_driver();

	if (D_get_cur_wind(cur_wind))
		G_fatal_error("No current window") ;
	
	if (argc == 2)
	{
		strcpy(new_wind, argv[1]) ;
	}
	else
	{
		printf( "\nButtons:\n") ;
		printf( "Left:   Identify a window\n") ;
		printf( "Middle: Quit without removing anything\n") ;
		printf( "Right:  Accept window\n") ;

		button = ident_win(new_wind) ;

		if (button == 2)
		{
			D_set_cur_wind(cur_wind) ;
			R_close_driver();
			exit(0) ;
		}
	}

	D_set_cur_wind(cur_wind) ;
	Dremove(new_wind) ;

	R_close_driver();
	exit(0);
}
