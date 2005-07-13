#include <stdlib.h>
#include "gis.h"

int get_stats (char *name, char *mapset, struct Cell_stats *statf, int quiet)
{
    int fd;
    CELL *cell;
    int row, nrows, ncols;

    fd = G_open_cell_old (name, mapset);
    if (fd < 0)
	exit(1);
    nrows = G_window_rows();
    ncols = G_window_cols();
    cell =  G_allocate_cell_buf();

    G_init_cell_stats (statf);
    if (!quiet) fprintf (stderr, "Reading %s ...", name);
    for (row = 0; row < nrows; row++)
    {
	if (!quiet) G_percent (row, nrows, 2);
	if (G_get_map_row (fd, cell, row) < 0)
	    break;
	G_update_cell_stats (cell, ncols, statf);
    }
    if (row < nrows)
	exit(1);
    G_close_cell (fd);
    G_free (cell);
    if (!quiet) G_percent (row, nrows, 2);

    return 0;
}
