
#include <string.h>
#include <math.h>
#include <grass/gis.h>
#include "driver.h"
#include "pngdriver.h"

static int *trans;
static int ncols;
static int nalloc;
static int src[2][2];
static int dst[2][2];

static double scale(double k, const int src[2], const int dst[2])
{
	return dst[0] + (double) (k - src[0]) * (dst[1] - dst[0]) / (src[1] - src[0]);
}

static int scale_fwd_y(int sy)
{
	return (int) floor(scale(sy, src[1], dst[1]) + 0.5);
}

static int scale_rev_x(int dx)
{
	return (int) floor(scale(dx + 0.5, dst[0], src[0]));
}

static int next_row(int sy, int dy)
{
	sy++;

	for (;;)
	{
		int y = scale_fwd_y(sy);
		if (y > dy)
			return sy - 1;
		sy++;
	}
}

static void alloc_buffers(void)
{
	if (nalloc >= ncols)
		return;

	nalloc = ncols;
	trans = G_realloc(trans, nalloc * sizeof(int));
}

void PNG_begin_scaled_raster(int s[2][2], int d[2][2])
{
	int i;

	ncols = d[0][1] - d[0][0];

	memcpy(src, s, sizeof(src));
	memcpy(dst, d, sizeof(dst));

	alloc_buffers();

	for (i = 0; i < ncols; i++)
		trans[i] = scale_rev_x(d[0][0] + i);
}

int PNG_scaled_raster(
	int n, int row,
	const unsigned char *red, const unsigned char *grn, const unsigned char *blu, const unsigned char *nul)
{
	int d_y0 = scale_fwd_y(row + 0);
	int d_y1 = scale_fwd_y(row + 1);
	int d_rows = d_y1 - d_y0;
	int x, y;

	if (d_rows <= 0)
		return next_row(row, d_y0);

	for (x = 0; x < ncols; x++)
	{
		int xx = dst[0][0] + x;
		int j = trans[x];
		int c;

		if (nul && nul[j])
			continue;

		c = PNG_lookup_color(red[j], grn[j], blu[j]);

		for (y = 0; y < d_rows; y++)
		{
			int yy = d_y0 + y;

			grid[yy * width + xx] = c;
		}
	}

	modified = 1;

	return next_row(row, d_y1);
}

