
/*  only function in this file being used at the moment is create_name().
*   Written by Grass Team, Fall of 88, -mh
*/

#include	<stdio.h>
#include	"gis.h"

#define		CHARNULL  '\0'

create_name( file_name, header, name, dir, file_desc)
	char	*file_name, *header,  *name,  *dir, *file_desc ;
{

	char	*mapset ;
	char	*G_mapset() ;

	if ( *name == CHARNULL)
		mapset = G_ask_new( header, name, dir, file_desc, 0) ;
	else
		mapset = G_mapset() ;

	if ( ! mapset)
		exit(0) ;

	G__file_name( file_name, dir, name, mapset) ;

}

ask_for_name( file_name, header, name, dir, file_desc)
	char	*file_name, *header,  *name,  *dir, *file_desc ;
{

	char	*mapset ;

	mapset = G_ask_new( header, name, dir, file_desc, 0) ;

	if ( ! mapset)
		exit(0) ;

	G__file_name( file_name, dir, name, mapset) ;

}	/**  ask_for_name()  **/


welcome_mat() 
{
	char  buf[81] ;

	printf("\n\n\n\n\n\n\n\n  This program will create a grid in a VECTOR file which can be digitized upon\n") ;
	printf("  or displayed upon the monitor. Remember 'support.vect' will have to be ran\n  before using the grid.\n\n\n\n") ;
	printf("\n  Information you will need to run this program:\n") ;
	printf("   1) Name of the vector file to store the grid in.\n") ;
	printf("   2) An easting and northing of the point in the lower left corner where the\n   grid will start from.\n") ;
	printf("\n   Note:  This only works for UTM mapsets.\n") ;

	printf("\n\n\n\n            Hit <RETURN> to continue ") ;
	gets(buf) ;
	printf("\n\n\n\n") ;
}



ask_for_int( num, quest)
	int	*num ;
	char	*quest ;
{

	char	buf[80] ;

	while(1)
	{
		printf("\n%s", quest) ;

		if (gets (buf) == NULL)
		{
			clearerr (stdin) ;
			exit (1) ;
		}
	
		*num = atoi(buf) ;
		if ( *num > 0)
			return(0) ;
		printf("\n Number has to be positive and greater then zero\n") ;
	}

}



ask_for_double( num, quest)
	double	*num ;
	char	*quest ;
{

	char	buf[80] ;
	double  atof() ;

	while(1)
	{
		printf("\n%s", quest) ;

		if (gets (buf) == NULL)
		{
			clearerr (stdin) ;
			exit (1) ;
		}

		*num = atof(buf) ;
		if ( *num > 0.0)
			return(0) ;
		printf("\n Number has to be positive and greater then zero\n") ;
	}

}



ask_for_string( buf, quest)
	char	*buf ;
	char	*quest ;
{

	while(1)
	{
		printf("\n%s", quest) ;
		if (gets (buf) == NULL)
		{
			clearerr (stdin) ;
			exit (1) ;
		}

		G_squeeze(buf) ;
		if( *buf != CHARNULL)
			return (0) ;

		printf("\n I need an answer before I can continue ( ^D or break key to quit)") ;

	}

	return(0) ;

}

