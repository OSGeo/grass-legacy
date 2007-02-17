
#include "pngdriver.h"

void PNG_RGB_raster(
	int num, int nrows,
	const unsigned char *r, const unsigned char *g, const unsigned char *b,
	const unsigned char *nul)
{
	int x, y;

	for (x = 0; x < num; x++)
	{
		int our_x = cur_x + x;
		int c;

		if (nul && nul[x])
			continue;

		c = PNG_lookup_color(r[x], g[x], b[x]);

		for (y = 0; y < nrows; y++)
		{
			int our_y = cur_y + y;

			grid[our_y * width + our_x] = c;
		}
	}

	modified = 1;
}

