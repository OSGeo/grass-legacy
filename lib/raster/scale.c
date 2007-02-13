
#include <string.h>
#include <math.h>
#include <grass/gis.h>
#include <grass/raster.h>

#if 1

static unsigned char *d_red, *d_grn, *d_blu, *d_nul;
static int *trans;
static int nalloc;
static int src[2][2];
static int dst[2][2];

static int scale(int k, const int src[2], const int dst[2])
{
	return (int) floor(0.5 + dst[0] + (double) (k - src[0]) * (dst[1] - dst[0]) / (src[1] - src[0]));
}

static int next_row(int sy, int dy, const int src[2], const int dst[2])
{
	sy++;

	for (;;)
	{
		int y = scale(sy, src, dst);
		if (y > dy)
			return sy - 1;
		sy++;
	}
}

static void alloc_buffers(int ncols)
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

void R_begin_scaled_raster(int s[2][2], int d[2][2])
{
	int i;

	memcpy(src, s, sizeof(src));
	memcpy(dst, d, sizeof(dst));

	alloc_buffers(dst[0][1]);

	for (i = dst[0][0]; i < dst[0][1]; i++)
		trans[i] = scale(i, dst[0], src[0]);
}

int R_scaled_raster(
	int row,
	unsigned char *red, unsigned char *grn, unsigned char *blu, unsigned char *nul)
{
	int d_width  = dst[0][1] - dst[0][0];
	int d_y0 = scale(row + 0, src[1], dst[1]);
	int d_y1 = scale(row + 1, src[1], dst[1]);
	int d_rows = d_y1 - d_y0;
	int i;

	if (d_rows <= 0)
		return next_row(row, d_y0, src[1], dst[1]);

	/* Allocate memory for raster */
	alloc_buffers(dst[0][1]);

	/* Make the screen raster */
	for (i = dst[0][0]; i < dst[0][1]; i++)
	{
		int j = trans[i];

		d_red[i] = red[j];
		d_grn[i] = grn[j];
		d_blu[i] = blu[j];
		if (nul)
			d_nul[i] = nul[j];
	}

	R_move_abs(dst[0][0], d_y0);
	R_RGB_raster(d_width, d_rows, d_red, d_grn, d_blu, nul ? d_nul : nul);

	return next_row(row, d_y1, src[1], dst[1]);
}

#else

int R_scaled_raster(
	int row,
	const int src[2][2], const int dst[2][2],
	unsigned char *red, unsigned char *grn, unsigned char *blu,
	unsigned char *nul)
{
	return trans->RGB_scaled_raster(row, src, dst, red, grn, blu, nul);
}

#endif
