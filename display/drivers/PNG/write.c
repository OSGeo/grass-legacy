
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <grass/gis.h>
#include "pngdriver.h"

void write_image(void)
{
	char *p = file_name + strlen(file_name) - 4;

	if (!modified)
		return;

	if (G_strcasecmp(p, ".png") == 0)
		write_png();
	else if (G_strcasecmp(p, ".ppm") == 0)
	{
		write_ppm();
		if (has_alpha)
			write_pgm();
	}
	else
		G_fatal_error("Graph_Close: unknown file type: %s", p);

	modified = 0;
}

