#include "cairodriver.h"

void Cairo_draw_bitmap(int ncols, int nrows, int threshold, const unsigned char *buf)
{
	cairo_surface_t *surf;

	G_debug(1, "Cairo_draw_bitmap: %d %d %d", ncols, nrows, threshold);

	surf = cairo_image_surface_create_for_data(
		(unsigned char *) buf, CAIRO_FORMAT_A8, ncols, nrows, ncols);

	if (cairo_surface_status(surf) != CAIRO_STATUS_SUCCESS)
		G_fatal_error("Cairo_draw_bitmap: Failed to create source");

	cairo_mask_surface(cairo, surf, cur_x, cur_y);

	cairo_surface_destroy(surf);
	modified = 1;
}

