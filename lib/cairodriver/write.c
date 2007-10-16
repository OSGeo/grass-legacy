#include "cairodriver.h"

void write_image(void)
{
	G_debug(1, "write_image");
	finish_drawing_op();
	if (cairo && surface && modified)
	{
		if (file_type == FTYPE_PNG)
		{
			G_debug(1, "Writing image to %s", file_name);
			cairo_surface_write_to_png(surface, file_name);
		}
		/* vector format files are written directly to file */
	}
	modified = 0;
}
