
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
#include    <stdio.h>
#include	"igraphics.h"
#include "driver.h"

extern int WNO ;
extern unsigned long VSI_PLANE_MASK ;
extern int I_COLOR_OFFSET;

extern short *Raster_Buffer;

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
	register int   i ;
	register int   y ;

	int cur_x, cur_y ;
	short begin_x, end_x ;

	Hide_cursor();

	Get_current_xy( &cur_x, &cur_y) ;

	begin_x = (short)cur_x ;
	end_x = (short)(cur_x+num - 1) ;

	if(num > ESTIMATED_MAX_SCREEN_WIDTH)
		num = MAX_SCREEN_WIDTH ;
	
	arr1 = array ;

	for ( y = 0; y < nrows; y++)
	{
		if (!overwrite)
			getline16 (WNO, (int)VSI_PLANE_MASK,
				begin_x, (short)(cur_y+y),
				end_x, (short)(cur_y+y),
				Raster_Buffer) ;


    /*  convert grass cat codes to color indexes  */
		if(color_type) /*grass cats*/
		{
			for ( i = 0; i < num; i++)
			{
			      if (overwrite || array[i])
				   Raster_Buffer[i] = (short)(_get_color_index (array[i])+ I_COLOR_OFFSET);
			}
		}
		else /*color indexes*/
		{
			for ( i = 0; i < num; i++)
			{
			      if (overwrite || array[i])
				   Raster_Buffer[i] = (short)(array[i]+ I_COLOR_OFFSET);
			}
		}

		putline16 (WNO, (int)VSI_PLANE_MASK,
			begin_x, (short)(cur_y+y),
			end_x, (short)(cur_y+y),
			Raster_Buffer) ;
	}


	Show_cursor();

	return(0) ;
}

static
do_nothing(n)
{
	return n ;
}

