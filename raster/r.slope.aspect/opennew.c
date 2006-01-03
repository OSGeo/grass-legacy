#include <stdlib.h>
#include "gis.h"

int opennew (char *name, RASTER_MAP_TYPE wr_type)
{
    int fd;
    char err[400];

    if (G_legal_filename (name) < 0)
    {
	sprintf (err, "%s - ** illegal name **", name);
	G_fatal_error (err);
	exit(1);
    }

    if(wr_type < 0) /* default fp type */
       fd = G_open_fp_cell_new (name);
    else
       fd = G_open_raster_new (name, wr_type);

    if (fd < 0)
    {
	sprintf (err, "failed in attempt to open %s\n", name);
	G_fatal_error (err);
	exit(1);
    }

    return fd;
}
