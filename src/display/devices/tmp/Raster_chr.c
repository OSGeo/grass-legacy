
/*
*  withzeros  -  if 0 it is overlay, 1 is not overlay.
*  colortype  -  if 0 it is color indexes, 1 is GRASS category codes.
* This writes a raster row.
* Even though there is a char drawline function we can't use it,
* because if it is GRASS category codes it can be converted to a int.
* 
*/

#include "igraphics.h"

static short short_array[ESTIMATED_MAX_SCREEN_WIDTH+10] ;

Raster_char(num, nrows, array, withzeros, color_type)
	int num ;
	int nrows ;
	unsigned char *array ;
	int withzeros ;
	int color_type ;
{

/* Copy char array to short array */
		
	register int i ;
	register short *sptr ;
	register unsigned char *cptr ;

	sptr = short_array ;
	cptr = array ;
	i = num ;
	while(i--)
	{
		*sptr = *cptr ;
		sptr++ ;   cptr++ ;
	}
	    
	Raster_short_def(num, nrows, short_array, withzeros, color_type) ;
}
