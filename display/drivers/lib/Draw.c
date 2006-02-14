#include "driver.h"
#include "driverlib.h"

void DRV_draw_bitmap(int ncols, int nrows, const unsigned char *buf)
{
	int i, j;

	if (driver->draw_bitmap)
	{
		(*driver->draw_bitmap)(ncols, nrows, buf);
		return;
	}

	if (!driver->draw_point)
		return;

	for (j = 0; j < nrows; j++)
		for (i = 0; i < ncols; i++)
			if (buf[j * ncols + i])
				(*driver->draw_point)(cur_x + i, cur_y + j);
}

void DRV_draw_line(int x0, int y0, int x1, int y1)
{
	if (driver->draw_line)
		(*driver->draw_line)(x0, y0, x1, y1);
}

void DRV_draw_point(int x, int y)
{
	if (driver->draw_point)
		(*driver->draw_point)(x, y);
}

