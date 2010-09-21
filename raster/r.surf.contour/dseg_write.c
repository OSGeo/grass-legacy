#include <grass/gis.h>
#include "seg.h"

static char *me = "dseg_write_cell";

int dseg_write_cellfile(DSEG * dseg, char *map_name)
{
    int map_fd;
    int row, nrows;
    char msg[100];
    DCELL *buffer;

    map_fd = G_open_raster_new(map_name, DCELL_TYPE);
    if (map_fd < 0) {
	sprintf(msg, "%s(): unable to open new map layer [%s]", me, map_name);
	G_warning(msg);
	return -1;
    }
    nrows = G_window_rows();
    buffer = G_allocate_d_raster_buf();
    for (row = 0; row < nrows; row++) {
	segment_get_row(&(dseg->seg), buffer, row);
	if (G_put_raster_row(map_fd, buffer, DCELL_TYPE) < 0) {
	    G_free(buffer);
	    G_unopen_cell(map_fd);
	    sprintf(msg,
		    "%s(): unable to write new map layer [%s], row %d",
		    me, map_name, row);
	    G_warning(msg);
	    return -2;
	}
    }
    G_free(buffer);
    G_close_cell(map_fd);
    return 0;
}
