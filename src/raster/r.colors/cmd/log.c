#include <stdlib.h>
#include "gis.h"
#include "local_proto.h"

/* generate log transformed color table for skewed datasets MN 8/2001 */

int log_grey_colors (char *name, char *mapset, struct Colors *colors, int quiet, int min, int max)
{
    struct Cell_stats statf;
    CELL *cell;
    int row, nrows, ncols;
    int fd;

    if ((fd = G_open_cell_old (name,mapset)) < 0)
	exit(1);
    cell = G_allocate_cell_buf();
    nrows = G_window_rows();
    ncols = G_window_cols();

    G_init_cell_stats (&statf);
    if (!quiet)
	fprintf (stderr, "Reading %s ...", name);
    for (row = 0; row < nrows; row++)
    {
	if (!quiet)
	    G_percent (row, nrows, 2);
	if (G_get_c_raster_row(fd, cell, row) < 0)
	    exit(1);
	G_update_cell_stats(cell, ncols, &statf);
    }
    if (!quiet)
	G_percent (row, nrows, 2);
    G_close_cell (fd);
    G_free (cell);
    G_make_histogram_log_colors (colors, &statf, min, max);
    G_free_cell_stats (&statf);
    
    return 0;
}
