
/*
*  Written by GRASS Team, Fall of 88,  Michael H.
*  cc  ll_to_secs.c  ../../libes/libcoorcnv.a
*/

static char rcsid[]="$Header$";

#include	<stdio.h>

static char  *PROG ;

main (argc, argv)
	int  argc ;
	char  *argv[] ;
{

	double  lat_sec ;
	double  lon_sec ;

	PROG = argv[0] ;
	setbuf(stdout, NULL) ;
	if( argc != 3)
	{
		printf(" Converts lat and lon to seconds.\n\n", PROG) ;
		printf(" Usage: %s  lat  lon\n", PROG) ;
		printf(" lat example:  44.22.30n\n") ;
		printf(" lon example:  103.52.30w\n") ;
		exit(-1) ;
	}

	CC_lat_scan( argv[1], &lat_sec) ;

	CC_lon_scan( argv[2], &lon_sec) ;

	printf("\n Lat: %lf,   Lon:  %lf\n\n", lat_sec, lon_sec) ;  


}		/*  main()  */

