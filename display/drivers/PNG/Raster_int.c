#include "pngdriver.h"

void PNG_Raster_int(int num, int nrows, const int *array, int withzeros, int color_type)
{
	void (*set_color)(int);
	int x, y;
	int c, r, g, b, r2, g2, b2, ind;

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

			ind = our_y * width + our_x;
			c = currentColor;
			if (transparency > 0.0) {
				DRV_lookup_rgb(grid[ind], &r, &g, &b);
				DRV_lookup_rgb(c, &r2, &g2, &b2);
				c = DRV_lookup_color(
					r*transparency+r2*(1-transparency),
					g*transparency+g2*(1-transparency),
					b*transparency+b2*(1-transparency));
			}
			grid[ind] = c;
		}
	}

	modified = 1;
}

