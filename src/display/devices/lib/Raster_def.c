#include "driver.h"
#include "driverlib.h"

static int do_nothing (int);

int Raster_int_def (int num, int nrows, int *array, int withzeros, int color_type)
{
	register int cur_color ;
	register int *arr ;
	register int npixles ;
	int our_x, our_y ;
	int (*assign_color)() ;
	
	if(color_type)
		assign_color = Color ;
	else
		assign_color = color ;

	arr = array ;
	(*assign_color)(cur_color = *array) ;

	npixles = 1 ;

	our_x = cur_x ;
	our_y = cur_y ;

	while (--num)
	{
		if (*(++arr) != cur_color)
		{
			if(withzeros || cur_color)
				Box_abs(our_x,
					our_y + nrows,
					our_x + npixles,
					our_y) ;
			our_x += npixles ;

			(*assign_color)(cur_color = *arr) ;
			npixles = 1 ;
		}
		else
		{
			npixles++ ;
		}
	}

	if(withzeros || cur_color)
		Box_abs(our_x,
			our_y + nrows,
			our_x + npixles,
			our_y) ;

	return 0;
}

static int do_nothing (int n)
{
	return n ;
}
