
#include <string.h>
#include <math.h>
#include <grass/gis.h>
#include "driver.h"

static unsigned char *d_red, *d_grn, *d_blu, *d_nul;
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
	d_red = G_realloc(d_red, nalloc);
	d_grn = G_realloc(d_grn, nalloc);
	d_blu = G_realloc(d_blu, nalloc);
	d_nul = G_realloc(d_nul, nalloc);
	trans = G_realloc(trans, nalloc * sizeof(int));
}

void LIB_begin_scaled_raster(int s[2][2], int d[2][2])
{
	int i;

	ncols = d[0][1] - d[0][0];

	memcpy(src, s, sizeof(src));
	memcpy(dst, d, sizeof(dst));

	alloc_buffers();

	for (i = 0; i < ncols; i++)
		trans[i] = scale_rev_x(d[0][0] + i);
}

int LIB_scaled_raster(
	int n, int row,
	unsigned char *red, unsigned char *grn, unsigned char *blu, unsigned char *nul)
{
	int d_y0 = scale_fwd_y(row + 0);
	int d_y1 = scale_fwd_y(row + 1);
	int d_rows = d_y1 - d_y0;
	int i;

	if (d_rows <= 0)
		return next_row(row, d_y0);

	/* Make the screen raster */
	for (i = 0; i < ncols; i++)
	{
		int j = trans[i];

		d_red[i] = red[j];
		d_grn[i] = grn[j];
		d_blu[i] = blu[j];
		if (nul)
			d_nul[i] = nul[j];
	}

	COM_Move_abs(dst[0][0], d_y0);
	COM_RGB_raster(ncols, d_rows, d_red, d_grn, d_blu, nul ? d_nul : nul);

	return next_row(row, d_y1);
}

