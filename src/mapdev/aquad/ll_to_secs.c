
/*
*  Written by GRASS Team, Fall of 88,  Michael H.
*  cc  ll_to_secs.c  ../../libes/libcoorcnv.a
*/

#include	<stdio.h>

static char  *PROG ;

main (argc, argv)
	int  argc ;
	char  *argv[] ;
{

	int     junk ;
	double  lat_sec ;
	double  lon_sec ;

	int    lat_hem ;
	int    lon_hem ;

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

/*  latitude is negative in the south and positive in the north.  */
	if ( lat_sec < 0)
		lat_hem = 0 ;
	else
		lat_hem = 1 ;

/*  lon is negative in the east and positive in the west.  */
	if ( lon_sec < 0)
		lon_hem = 0 ;
	else
		lon_hem = 1 ;

	printf("\n\n Lat: %lf,   Lon: %lf\n", lat_sec, lon_sec) ;  
	printf(" In the %s-%s hemisphere.\n\n",
		lat_hem ? "north" : "south" ,
		lon_hem ? "west" : "east" ) ;  


}		/*  main()  */

