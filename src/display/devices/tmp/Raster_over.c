
/*  
*  This is Raster_int_def() from ../lib.  Array is 'shorts'.
*  Overwrites are being done by Raster_short_def().
*/

#include "driver.h"


Raster_overlay(num, nrows, array)
	int num ;
	int nrows ;
	short *array ;
{
	register short cur_color ;
	register short *arr ;
	register int npixles ;
	short no_data ;
	int our_x, our_y ;
	int color() ;  /* device color call (generally useful for fixed only) */

	no_data = (short) _get_color_index((int)0) ;

	arr = array ;
	cur_color = *arr ;
	color((int)cur_color) ;

	npixles = 0 ;

	our_x = cur_x ;
	our_y = cur_y ;

	while (--num)
	{
		if (*(++arr) != cur_color)
		{
			if (nrows == 1)
			{
				if( cur_color != no_data)
					Cont_rel(npixles,0) ;
				else
					Move_rel(npixles,0) ;
				cur_x++ ;
			}
			else
			{
				if(cur_color != no_data)
					Box_abs(our_x,
					   our_y + nrows,
					   our_x + npixles,
					   our_y) ;
				our_x += npixles ;
			}
			our_x++ ;

			cur_color = *arr ;
			color((int)cur_color) ;
			npixles = 0 ;
		}
		else
		{
			npixles++ ;
		}
	}

	if (nrows == 1)
	{
		if( cur_color != no_data)
			Cont_rel(npixles,0) ;
		else
			Move_rel(npixles,0) ;
		cur_x++ ;
	}
	else
	{
		if(cur_color != no_data)
			Box_abs(our_x,
			   our_y + nrows,
			   our_x + npixles,
			   our_y) ;
		cur_y = our_y + nrows ;
		cur_x = our_y + npixles ;
	}


}

static
do_nothing(n)
{
	return n ;
}
