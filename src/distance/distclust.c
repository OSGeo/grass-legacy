/* %W% %G% */

/* This is very OLD code - don't try to compile it and expect it work */

#include <stdio.h>
#include "gis.h"
#define FGET	fgets(buffer, 80, distin)
#define DIST_IN	"/usr/gis/tmp/dist_in"

main(argc, argv)
	int argc ;
	char *argv[] ;
{
	FILE *fopen(), *distin ;
	int disfile ;
	char buffer[80] ;
	int dist[256] ;
	int j,k ;
	int temp ;
	int numdist = 0 ;
	int incr ;
	int n_read ;
	int atcell ;
	unsigned char cellbuff[512] ;

	G_gisinit("DISTCLUST") ;

	if (argc != 2)
	{
		fprintf(stderr,"ERROR: USE  distclust <fileneme>") ;
		sleep(3) ;
		exit(-1) ;
	}

/*	sprintf(buffer,"%s/cell/%s",G_getenv("LOCATION"), argv[1]) ;  */
	if ((disfile=open(argv[1],2)) == -1)
	{
		fprintf(stderr,"ERROR: Can't open %s\n", argv[1]) ;
		sleep(3) ;
		exit(-1) ;
	}

	if ((distin=fopen(DIST_IN,"r")) == NULL)
	{
		fprintf(stderr,"ERROR: opening %s\n", DIST_IN) ;
		sleep(3) ;
		return(-1) ;
	}

	dist[numdist] = 0 ;
	numdist++ ;

	FGET ; FGET ;
	while(FGET != NULL)
	{
		sscanf(buffer,"%d",&dist[numdist]) ;
		numdist++ ;
	}

	for (k=0; k<numdist-1; k++)
	{
		if ( dist[k] > dist[k+1] )
		{
			temp = dist[k+1] ;
			for (j=k; j>=0 && dist[j]>temp; j--)
				dist[j+1] = dist[j] ;
			dist[j+1] = temp ;
		}
	}

	lseek(disfile, 0L, 0) ;
	while(1) 
	{
		n_read = read(disfile, cellbuff, 512) ;
		for(atcell=0; atcell<n_read; atcell++)
		{
			for(k=0; k<numdist && cellbuff[atcell]>dist[k] ; k++) ;
			if (k==numdist)
				cellbuff[atcell] = 0 ;
			else
				cellbuff[atcell] = k ;
		}
		lseek(disfile, -n_read, 1) ;
		if (write(disfile, cellbuff, n_read) != n_read)
		{
			fprintf(stderr,"ERROR: writing to cellfile\n") ;
			sleep(3) ;
			exit(-1) ;
		}
		if (n_read != 512) break ;
	}

	G_close_cell(disfile) ;
	fclose(distin) ;
	exit(0) ;
}
