
/*
*  Written by GRASS Team, Summer of 89,  -mh
*    ../../libes/libcoorcnv.a
*/

#include	<stdio.h>
#include	"quad.h"

/*
* argv[0] the program name (of course)
* argv[1] latitude
* argv[2] longitude
*/

static char  *PROG ;

main (argc, argv)
	int  argc ;
	char  *argv[] ;
{

	struct  quads_description  quads_info ;


	PROG = argv[0] ;
	setbuf(stdout, NULL) ;
	if( argc != 3)
	{
		fprintf (stdout,"\n Finds the closest quad to the given point.\n\n") ;
		fprintf (stdout," Usage: %s  lat  lon\n", PROG) ;
		fprintf (stdout,"   Example: %s  44.22.30n  103.52.30w\n\n", PROG) ;
		exit(-1) ;
	}

	setup_ll_to_utm( &quads_info) ;

	if ( init_quad_struct( &quads_info, argv[1], argv[2]))
	{
		fprintf (stdout,"\n ERROR: Could not convert the given lat/lon.\n") ;
		exit(-1) ;
	}

	if (find_quad_point( &quads_info))
	{
		print_quad_error( &quads_info) ;
		exit(-1) ;
	}

	print_quad( &quads_info) ;

	exit(0) ;
	
}		/*  main()  */

