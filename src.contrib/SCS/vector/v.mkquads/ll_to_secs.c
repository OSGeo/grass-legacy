
/*
*  Written by GRASS Team, Fall of 88,  Michael H.
*  cc  ll_to_secs.c  ../../libes/libcoorcnv.a
*/

static char rcsid[]="$Header$";

#include	<stdio.h>

static char  *PROG ;

int 
main (int argc, char *argv[])
{

	double  lat_sec ;
	double  lon_sec ;

	PROG = argv[0] ;
	setbuf(stdout, NULL) ;
	if( argc != 3)
	{
		fprintf (stdout," Converts lat and lon to seconds.\n\n", PROG) ;
		fprintf (stdout," Usage: %s  lat  lon\n", PROG) ;
		fprintf (stdout," lat example:  44.22.30n\n") ;
		fprintf (stdout," lon example:  103.52.30w\n") ;
		exit(-1) ;
	}

	CC_lat_scan( argv[1], &lat_sec) ;

	CC_lon_scan( argv[2], &lon_sec) ;

	fprintf (stdout,"\n Lat: %lf,   Lon:  %lf\n\n", lat_sec, lon_sec) ;  


}		/*  main()  */

