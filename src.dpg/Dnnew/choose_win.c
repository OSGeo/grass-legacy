/*  %W%  %G%  */

#include <stdio.h>
/*
 *   Dchoose
 *
 *   Usage:  Dchoose name
 *           Dchoose     (choose with mouse if on tty)
 *
 *   Choose a window on the screen
 */

main(argc, argv)
	char *argv[];
{
	char orig_win[64] ;
	char cur_pad[64] ;
	int stat ;
	int button ;

	if (argc > 2)
	{
		D_usage(argv[0], "name") ;
		D_usage(argv[0], "   (to use mouse)") ;
		exit(1);
	}

	R_open_driver ();

	if (argc == 2)
	{
		if(stat = Dchoose(argv[1]))
			printf("Error setting current window\n") ;
		R_close_driver ();
		exit(stat) ;
	}

/* Save current window just in case */
	D_get_cur_wind(orig_win) ;

	fprintf(stderr, "\nButtons:\n") ;
	fprintf(stderr, "Left:   Identify a window\n") ;
	fprintf(stderr, "Middle: Keep original window\n") ;
	fprintf(stderr, "Right:  Accept window\n") ;

	button = ident_win(cur_pad) ;

	if (button == 2)
		D_set_cur_wind(orig_win) ;
	
	D_timestamp() ;

	R_close_driver() ;

	printf("%s\n", cur_pad) ;
	exit(0);
}
