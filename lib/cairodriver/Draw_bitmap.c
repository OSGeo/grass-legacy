#include "cairodriver.h"

void Cairo_draw_bitmap(int ncols, int nrows, int threshold, const unsigned char *buf)
{
	G_debug(1, "Cairo_draw_bitmap: %d %d %d", ncols, nrows, threshold);

	/*
	   // TODO: support... the code below probably doesn't really work.

	   finish_drawing_op();

	   / * create source Cairo surface (assumes truecolor ARGB format) * /
	   cairo_surface_t *srcSurface = cairo_image_surface_create_for_data
	   (buf, CAIRO_FORMAT_ARGB32, ncols, nrows, ncols*BYTES_PER_PIXEL);
	   if (cairo_surface_status(srcSurface) != CAIRO_STATUS_SUCCESS) {
	   G_fatal_error("Cairo_draw_bitmap: Failed to create source");
	   }

	   / * paint the surface at position 0, 0 * /
	   cairo_set_source_surface(cairo, srcSurface, 0, 0);
	   cairo_paint(cairo);

	   cairo_surface_destroy(srcSurface);
	   modified = 1;
	 */
}
