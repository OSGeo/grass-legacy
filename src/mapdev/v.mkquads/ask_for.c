
/*  only function in this file being used at the moment is create_name().
*   Written by Grass Team, Fall of 88, -mh
*/

#include	<stdio.h>
#include <stdlib.h>
#include	"gis.h"

#define		CHARNULL  '\0'

int create_name (char *file_name, char *header, char *name,
	char *dir, char *file_desc)
{
	char	*mapset ;

	if ( *name == CHARNULL)
		mapset = G_ask_new( header, name, dir, file_desc) ;
	else
		mapset = G_mapset() ;

	if ( ! mapset)
		exit(0) ;

	G__file_name( file_name, dir, name, mapset) ;

	return (0);
}

int 
ask_for_name (char *file_name, char *header, char *name, char *dir, char *file_desc)
{

	char	*mapset ;

	mapset = G_ask_new( header, name, dir, file_desc) ;

	if ( ! mapset)
		exit(0) ;

	G__file_name( file_name, dir, name, mapset) ;

	return (0);
}	/**  ask_for_name()  **/


int 
welcome_mat (void) 
{
	char  buf[81] ;

	fprintf (stdout,"\n\n\n\n\n\n\n\n  This program will create a grid in a VECTOR file which can be digitized upon\n") ;
	fprintf (stdout,"  or displayed upon the monitor. Remember 'support.vect' will have to be ran\n  before using the grid.\n\n\n\n") ;
	fprintf (stdout,"\n  Information you will need to run this program:\n") ;
	fprintf (stdout,"   1) Name of the vector file to store the grid in.\n") ;
	fprintf (stdout,"   2) An easting and northing of the point in the lower left corner where the\n   grid will start from.\n") ;
	fprintf (stdout,"\n   Note:  This only works for UTM mapsets.\n") ;

	fprintf (stdout,"\n\n\n\n            Hit <RETURN> to continue ") ;
	fgets(buf,80,stdin) ;
	fprintf (stdout,"\n\n\n\n") ;

	return (0);
}



int 
ask_for_int (int *num, char *quest)
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



int 
ask_for_double (double *num, char *quest)
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

		*num = atof(buf) ;
		if ( *num > 0.0)
			return(0) ;
		fprintf (stdout,"\n Number has to be positive and greater then zero\n") ;
	}

	return 0;
}



int 
ask_for_string (char *buf, char *quest)
{

	while(1)
	{
		fprintf (stdout,"\n%s", quest) ;
		if (fgets (buf,80,stdin) == NULL)
		{
			clearerr (stdin) ;
			exit (1) ;
		}

		G_squeeze(buf) ;
		if( *buf != CHARNULL)
			return (0) ;

		fprintf (stdout,"\n I need an answer before I can continue ( ^D or break key to quit)") ;

	}

	return(0) ;
}
