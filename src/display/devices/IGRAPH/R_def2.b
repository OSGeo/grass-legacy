/*
*  overwrite  -  if 0 it is overlay, 1 it is overwrite.
*  colortype  -  if 0 it is color indexes, 1 is GRASS category codes.
* This writes a raster row.
* Even though there is a char drawline function we can't use it,
* because if it is GRASS category codes it can be converted to a int.
* 
*  Note:  i changed withzeros to overwrite, because it was more logical.
*/

#include	<tools.h>
#include "driver.h"

extern int WNO ;
extern unsigned long VSI_PLANE_MASK ;

/*  Debug tools  */
char buf[100] ;


Raster_short_def(num, nrows, array, overwrite, color_type)
	int num ;
	int nrows ;
	short *array ;
	int overwrite ;
	int color_type ;
{
	register short *arr1 ;
	register short *arr2 ;
	register int   i ;
	register int   y ;

	int cur_x, cur_y ;
	short begin_x, end_x ;


	Get_current_xy( &cur_x, &cur_y) ;

	if(num > MAX_SCREEN_WIDTH)
		num = MAX_SCREEN_WIDTH ;
	
	arr1 = array ;
	arr2 = array ;

    /*  convert grass cat codes to color indexes  */
	if(color_type)
		for ( i = 0; i < num; ++i)
		{
			*arr1 = (short) _get_color_index((int)(*arr1)) ;
			arr1++;
		}


    /* not an over write it must be an overlay  */
	if( ! overwrite)
	{
		/*
		Raster_overlay(num, nrows, array) ;
		*/
		return(0) ;
	}

	begin_x = (short)cur_x ;
	end_x = (short)(cur_x+num - 1) ;

/**
sprintf(buf, " begin_x: %d, c_y: %d,   num: %d, a22: %d",
	begin_x, cur_y, num, array[22]) ;
write_debug(buf) ;
**/

/**  there are two types of raster draws: putline and putpixelblock */

	for ( y = 0; y < nrows; ++y)
	{
		putline16 (WNO, (int)VSI_PLANE_MASK,
			begin_x, (short)(cur_y+y),
			end_x, (short)(cur_y+y),
			array) ;
	}

	return(0) ;
}

static
do_nothing(n)
{
	return n ;
}

