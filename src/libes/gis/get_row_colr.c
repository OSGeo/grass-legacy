
#include "gis.h"
#include "G.h"

int
G_get_raster_row_colors(
	int fd, int row, struct Colors *colors,
	unsigned char *red, unsigned char *grn, unsigned char *blu)
{
	static void *array;
	static int array_size;
	static char *set;
	static int set_size;

	int cols = G__.window.cols;
	int type = G__.fileinfo[fd].map_type;
	int size = G_raster_size(type);

	if (array_size < cols * size)
	{
		array_size = cols * size;
		array = (DCELL *) G_realloc(array, array_size);
	}

	if (set_size < cols)
	{
		set_size = cols;
		set = (unsigned char *) G_realloc(set, set_size);
	}

	if (G_get_raster_row(fd, array, row, type) < 0)
		return -1;

	G_lookup_raster_colors(array, red, grn, blu, set, cols, colors, type);

	return 0;
}

