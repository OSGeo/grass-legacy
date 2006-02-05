
#include "pngdriver.h"

void PNG_Raster_int(int num, int nrows, const int *array, int withzeros, int color_type)
{
	void (*set_color)(int);
	int x, y;
	
	set_color = color_type
		? COM_Color
		: DRV_color;

	for (x = 0; x < num; x++)
	{
		int c = array[x];
		int our_x = cur_x + x;

		if (!c && !withzeros)
			continue;

		(*set_color)(c);

		for (y = 0; y < nrows; y++)
		{
			int our_y = cur_y + y;

			grid[our_y * width + our_x] = currentColor;
		}
	}

	modified = 1;
}

