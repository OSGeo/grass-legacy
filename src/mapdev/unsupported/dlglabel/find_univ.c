/*  @(#)find_univ.c	2.1  6/26/87  */
/*  file contains  find_univ_info()
*/

#include	 "dlg.h"
#include	 <stdio.h>


/*  Check to see if there is a universe box
*  area one should have the attributes of major = 0,  minor = 0 and define
*  the area outside the map.
*/

find_univ_info()
{

	char	buf[30] ;

	universe_defined = 0 ;
	if (area[1].n_atts > 0)
	{
		if ( ! area[1].atts[0] && ! area[1].atts[1] )
		{
			universe_defined = 1 ;
			return ;
		}
		printf( "\n Universe not defined by dlg-3 standards.\n") ;
		printf( " Is the universe defined (y,n): ") ;

		if (gets(buf) == NULL)
		{
			clearerr(stdin) ;
			return ;
		}

		if ( *buf == 'y')
			universe_defined = 1 ;

	}

}
		/*  find_univ_info()  */

