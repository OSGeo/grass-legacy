

#include	<stdio.h>
#include	"gis.h"


welcome_mat() 
{
	char  buf[81] ;

	printf("\n\n\n\n\n\n\n\n  This program will create a grid in a VECTOR file which can be digitized upon\n") ;
	printf("  or displayed upon the monitor. Remember 'support.vect' will have to be run\n  before using the grid.\n\n\n\n") ;
	printf("\n  Information you will need to run this program:\n") ;
	printf("   1) Name of the vector file to store the grid.\n") ;
	printf("   2) The number of rows and columns in the grid.\n") ;
	printf("   2) An easting and northing of the point in the lower left corner where the\n   grid will start.\n") ;
	printf("   3) The width and length of the boxes in the grid.\n") ;

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


ask_for_name( file_name, header, name, dir, file_desc)
	char	*file_name, *header,  *name,  *dir, *file_desc ;
{

	char	*mapset ;

	mapset = G_ask_new( header, name, dir, file_desc, 0) ;

	if ( ! mapset)
		exit(0) ;

	G__file_name( file_name, dir, name, mapset) ;

}	/**  ask_for_name()  **/

