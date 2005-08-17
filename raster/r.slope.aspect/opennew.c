#include <stdlib.h>
#include "gis.h"


int opennew (char *name, RASTER_MAP_TYPE wr_type)
{
    int fd;

    if (G_legal_filename (name) < 0)
	G_fatal_error ("%s - ** illegal name **", name);

    if(wr_type < 0) /* default fp type */
       fd = G_open_fp_cell_new (name);
    else
       fd = G_open_raster_new (name, wr_type);

    if (fd < 0)
	G_fatal_error ("failed in attempt to open %s\n", name);

    return fd;
}
