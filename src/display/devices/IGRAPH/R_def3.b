/*
*  overwrite  -  if 0 it is overlay, 1 it is overwrite.
*  colortype  -  if 0 it is color indexes, 1 is GRASS category codes.
* This writes a raster row.
* Even though there is a char drawline function we can't use it,
* because if it is GRASS category codes it can be converted to a int.
* 
*  Note:  i changed withzeros to overwrite, because it was more logical.
*/

#include "driver.h"

Raster_short_def(num, nrows, array, overwrite, color_type)
	int num ;
	int nrows ;
	short *array ;
	int overwrite ;
	int color_type ;
{
	register short cur_color ;
	register short *arr ;
	register int npixles ;
	int our_x, our_y ;
	int (*assign_color)() ;
	int Color() ;  /* GRASS driver color call */
	int color() ;  /* device color call (generally useful for fixed only) */
	
	if(color_type)
		assign_color = Color ;
	else
		assign_color = color ;

	arr = array ;
	cur_color = *array ;
	(*assign_color)((int)cur_color) ;

	npixles = 0 ;

	our_x = cur_x ;
	our_y = cur_y ;

	while (--num)
	{
		arr += 1 ; ;
		if (*arr != cur_color)
		{
			if (nrows == 1)
			{
				if(overwrite || cur_color)
					Cont_rel(npixles,0) ;
				else
					Move_rel(npixles,0) ;
				cur_x++ ;
			}
			else
			{
				if(overwrite || cur_color)
					Box_abs(our_x,
					   our_y + nrows,
					   our_x + npixles,
					   our_y) ;
				our_x += npixles ;
			}
			our_x++ ;

			cur_color = *arr ;
			(*assign_color)((int)cur_color) ;
			npixles = 0 ;
		}
		else
		{
			npixles++ ;
		}
	}

	if (nrows == 1)
	{
		if(overwrite || cur_color)
			Cont_rel(npixles,0) ;
		else
			Move_rel(npixles,0) ;
		cur_x++ ;
	}
	else
	{
		if(overwrite || cur_color)
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
