
/*
*  withzeros  -  if 0 it is overlay, 1 is not overlay.
*  colortype  -  if 0 it is color indexes, 1 is GRASS category codes.
* 
*/

#include "igraphics.h"

static short short_array[ESTIMATED_MAX_SCREEN_WIDTH+10] ;

Raster_int(num, nrows, array, withzeros, color_type)
	int num ;
	int nrows ;
	unsigned int *array ;
	int withzeros ;
	int color_type ;
{

/* Copy int array to short array */
		
	register int i ;
	register short *sptr ;
	register unsigned int *cptr ;

	sptr = short_array ;
	cptr = array ;
	i = num ;
	while(i--)
	{
		*sptr = (short)(*cptr) ;
		sptr++ ;  cptr++ ;
	}
	    
	Raster_short_def(num, nrows, short_array, withzeros, color_type) ;
}
