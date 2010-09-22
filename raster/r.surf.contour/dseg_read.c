#include <grass/gis.h>
#include <unistd.h>
#include "seg.h"

static char *me = "dseg_read_cell";

int dseg_read_cell(DSEG * dseg, char *map_name, char *mapset)
{
    int row, nrows;
    int map_fd;
    char msg[100];
    DCELL *buffer;

    dseg->name = NULL;
    dseg->mapset = NULL;

    if ((map_fd = G_open_cell_old(map_name, mapset)) < 0) {
	sprintf(msg, "%s(): unable to open file [%s] in [%s]",
		me, map_name, mapset);
	G_warning(msg);
	return -3;
    }
    nrows = G_window_rows();
    buffer = G_allocate_d_raster_buf();
    for (row = 0; row < nrows; row++) {
	if (G_get_d_raster_row(map_fd, buffer, row) < 0) {
	    G_free(buffer);
	    G_close_cell(map_fd);
	    sprintf(msg, "%s(): unable to read file [%s] in [%s]",
		    me, map_name, mapset);
	    G_warning(msg);
	    return -2;
	}
	if (segment_put_row(&(dseg->seg), buffer, row) < 0) {
	    G_free(buffer);
	    G_close_cell(map_fd);
	    sprintf(msg, "%s(): unable to segment put row for [%s] in [%s]",
		    me, map_name, mapset);
	    G_warning(msg);
	    return (-1);
	}
    }

    G_close_cell(map_fd);
    G_free(buffer);

    dseg->name = G_store(map_name);
    dseg->mapset = G_store(mapset);

    return 0;
}
