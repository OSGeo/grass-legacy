#include "driver.h"
#include "driverlib.h"

void COM_Raster_int(int num, int nrows, const int *array, int withzeros, int color_type)
{
	int cur_color;
	const int *arr;
	int npixels;
	int our_x, our_y;
	void (*assign_color)(int);

	if (driver->Raster_int)
	{
		(*driver->Raster_int)(num, nrows, array, withzeros, color_type);
		return;
	}

	assign_color = color_type ? COM_Color : DRV_color;

	arr = array;
	cur_color = *array;

	(*assign_color)(cur_color);

	npixels = 1;

	our_x = cur_x;
	our_y = cur_y;

	while (--num)
	{
		if (*(++arr) != cur_color)
		{
			if (withzeros || cur_color)
				COM_Box_abs(
					our_x, our_y + nrows,
					our_x + npixels, our_y);
			our_x += npixels;

			cur_color = *arr;
			(*assign_color)(cur_color);
			npixels = 1;
		}
		else
			npixels++;
	}

	if (withzeros || cur_color)
		COM_Box_abs(
			our_x, our_y + nrows,
			our_x + npixels, our_y);
}

