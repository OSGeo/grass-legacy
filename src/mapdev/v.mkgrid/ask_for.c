#include	<stdio.h>
#include <stdlib.h>
#include	"gis.h"

int welcome_mat (void) 
{
	char  buf[81] ;

	fprintf (stdout,"\n\n\n\n\n\n\n\n  This program will create a grid in a VECTOR file which can be digitized upon\n") ;
	fprintf (stdout,"  or displayed upon the monitor. Remember v.support will have to be run\n  before using the grid.\n\n\n\n") ;
	fprintf (stdout,"\n  Information you will need to run this program:\n") ;
	fprintf (stdout,"   1) Name of the vector file to store the grid.\n") ;
	fprintf (stdout,"   2) The number of rows and columns in the grid.\n") ;
	fprintf (stdout,"   3) An easting and northing of the point in the lower left corner where the\n      grid will start.\n") ;
	fprintf (stdout,"   4) The width and length of the boxes in the grid.\n") ;
	fprintf (stdout,"   5) The angle of rotation counter-clockwise about the origin.\n") ;
	fprintf (stdout,"   6) The type of attribute grid if applicable. \n      Options are [const, rows, cols].\n") ;
	fprintf (stdout,"   7) If a constant attribute is selected, the value may also be set. \n      Default 1.\n") ;

	fprintf (stdout,"\n\n\n\n            Hit <RETURN> to continue ") ;
	fgets(buf,81,stdin) ;
	fprintf (stdout,"\n\n\n\n") ;

	return 0;
}

int ask_for_int (int *num, char *quest)
{

	char	buf[80] ;

	while(1)
	{
		fprintf (stdout,"\n%s", quest) ;

		if (fgets (buf,80,stdin) == NULL)
		{
			clearerr (stdin) ;
			exit (1) ;
		}
	
		*num = atoi(buf) ;
		if ( *num > 0)
			return(0) ;
		fprintf (stdout,"\n Number has to be positive and greater then zero\n") ;
	}

	return 0;
}

int ask_for_double (double *num, char *quest)
{

	char	buf[80] ;
	double  atof() ;

	while(1)
	{
		fprintf (stdout,"\n%s", quest) ;

		if (fgets (buf,80,stdin) == NULL)
		{
			clearerr (stdin) ;
			exit (1) ;
		}

		/* *num = atof(buf) ;*/
		if(sscanf(buf, "%lf", num)==1)
			return(0) ;
		fprintf (stdout,"\n Illegal input!\n") ;
	}

	return 0;
}

int ask_for_name (char *file_name, char *header,
	char *name, char *dir, char *desc)
{
	char	*mapset ;

	mapset = G_ask_new( header, file_name, dir, desc) ;

	if ( ! mapset)
		exit(0) ;

/*	G__file_name( file_name, dir, name, mapset) ;*/

	return 0;
}	/**  ask_for_name()  **/
