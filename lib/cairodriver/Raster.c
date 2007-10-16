#include "cairodriver.h"

static int srcTop, srcBottom, srcLeft, srcRight, srcWidth, srcHeight;
static int destTop, destBottom, destLeft, destRight, destWidth, destHeight;

static cairo_surface_t *srcSurface;
static unsigned char *srcSurfaceData;
static int srcSurfaceStride;

void Cairo_begin_scaled_raster(int mask, int s[2][2], int d[2][2])
{
	G_debug(1, "Cairo_begin_scaled_raster: %d %d %d %d %d %d %d %d %d",
		mask, s[0][0], s[0][1], s[1][0], s[1][1], d[0][0], d[0][1], d[1][0], d[1][1]);
	finish_drawing_op();

	/* TODO: are top and left swapped? */

	srcRight = s[0][1];
	srcBottom = s[1][1];
	srcLeft = s[0][0];
	srcTop = s[1][0];
	destRight = d[0][1];
	destBottom = d[1][1];
	destLeft = d[0][0];
	destTop = d[1][0];

	srcHeight = srcBottom - srcTop + 1;
	srcWidth = srcRight - srcLeft + 1;
	destHeight = destBottom - destTop + 1;
	destWidth = destRight - destLeft + 1;

	G_debug(1, " src (TBLR): %d %d %d %d, dst (TBLR) %d %d %d %d",
		srcTop, srcBottom, srcLeft, srcRight, destTop, destBottom, destLeft, destRight);

	/* create source surface */
	srcSurface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, srcWidth, srcHeight);
	if (cairo_surface_status(srcSurface) != CAIRO_STATUS_SUCCESS)
	{
		G_fatal_error("Cairo_begin_scaled_raster: Failed to create surface");
	}
	srcSurfaceData = cairo_image_surface_get_data(srcSurface);
	srcSurfaceStride = cairo_image_surface_get_stride(srcSurface);
}

int Cairo_scaled_raster(
	int n, int row,
	const unsigned char *red, const unsigned char *green, const unsigned char *blue, const unsigned char *nul)
{
	unsigned int *dst = (unsigned int *) srcSurfaceData;
	int col;

	/* TODO: n is number of pixels in row? */
	/* TODO: nul array is mask? */

	G_debug(3, "Cairo_scaled_raster: %d %d", n, row);

	dst += row * (srcSurfaceStride >> 2);

	for (col = 0; col < srcWidth; col++)
		*dst++ = 0xFF000000 + (*red++ << 16) + (*green++ << 8) + *blue++;

	return row + 1;
}

void Cairo_end_scaled_raster(void)
{
	G_debug(1, "Cairo_end_scaled_raster");

	/* paint source surface onto destination (scaled) */
	cairo_save(cairo);
	cairo_scale(cairo, (double) destWidth / (double) srcWidth, (double) destHeight / (double) srcHeight);
	cairo_set_source_surface(cairo, srcSurface, destLeft, destTop);
	cairo_paint(cairo);
	cairo_restore(cairo);

	/* cleanup */
	cairo_surface_destroy(srcSurface);
	modified = 1;
}
