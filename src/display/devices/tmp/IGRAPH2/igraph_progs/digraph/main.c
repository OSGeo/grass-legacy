

/*
*  
*  Written by the GRASS Team in the Spring of 90, -mh.
*/

#include	<stdio.h>
#include	<tools.h>
#include	"gis.h"
#include	"env.h"


main( argc, argv)
	int  argc ;
	char **argv ;
{

	struct  screen_description  desc ;

/*  Do arg checking for different driver name  */
switch (argc)
{
	case 1:
		desc.driver_name = DEFAULT_DRIVER ;
		break ;

	case 2:
		desc.driver_name = argv[1] ;
		break ;

	default:
		fprintf( stderr, " Usage: %s [driver name]\n", argv[0]) ;
		fprintf( stderr, " If no driver name is given the default driver will be used [%s]\n", DEFAULT_DRIVER ) ;
		exit(-1) ;
		break ;

}

	G_gisinit(argv[0]) ;

	Enter_tools() ;

	Inq_displayed_vs(&(desc.current_vs_no)) ;

	setbuf(stdout, NULL) ;

	Initialize_curses() ;

	get_screen_parameters( &desc) ;
	print_description( &desc) ;
	print_current_parameters( &desc) ;

	/*
	*  This wait has to be here or the pull_down menu will
	*  take so much of the system resources that the curses
	*  screen will not be displayed.
	*/
	Wait_timer(30) ;

	pull_down_menu( &desc);

	Close_curses() ;
	Exit_tools() ;

}  /*  eof of main  */

