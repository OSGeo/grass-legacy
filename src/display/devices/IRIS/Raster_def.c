
#include "driver.h"
#include "iris.h"

extern short *Raster_Buffer;
extern int SCREEN_BOTTOM;

Raster_int_def (num, nrows, array, withzeros, color_type)
	int num ;
	int nrows ;
	unsigned int *array ;
	int withzeros ;
	int color_type ;
{
	register unsigned int cur_color ;
	register unsigned int *arr ;
	register int npixles ;
	int our_x, our_y ;
	int i;
	

	npixles = 0 ;

	our_x = cur_x ;
	our_y = cur_y ;

	if (!withzeros)
	    rectread (cur_x, SCREEN_BOTTOM - cur_y, cur_x + num-1, SCREEN_BOTTOM - cur_y, Raster_Buffer);

	if (color_type)
	{
	    for (i = 0 ; i < num ; i++)
		if (withzeros || array[i])
		    Raster_Buffer[i] = _get_color_index (array[i]) + COLOR_OFFSET;
	}
	else
	{
	    for (i = 0 ; i < num ; i++)
		if (withzeros || array[i])
		    Raster_Buffer[i] = array[i] + COLOR_OFFSET;
	}


	for (i = cur_y ; i <= cur_y + nrows-1 ; i++)
	{
	    rectwrite (cur_x, SCREEN_BOTTOM - i, cur_x + num-1, SCREEN_BOTTOM - i, Raster_Buffer);
	}


	cur_y += (nrows-1);
	/*
	cur_x += num;
	*/
}

static
do_nothing(n)
{
	return n ;
}
