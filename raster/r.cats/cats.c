#include <stdlib.h>
#include "gis.h"

static struct Cell_stats statf;

int get_cats (char *name, char *mapset)
{
    int fd;
    int row, nrows, ncols;
    CELL *cell;
    struct Cell_head cellhd;

/* set the window to the cell header */
    if(G_get_cellhd (name, mapset, &cellhd) < 0)
	exit(1);
    G_set_window (&cellhd);

/* open the cell file */
    fd = G_open_cell_old (name, mapset);
    if (fd < 0)
	exit(1);
    nrows = G_window_rows();
    ncols = G_window_cols();
    cell = G_allocate_cell_buf();
    G_init_cell_stats (&statf);

/* read the cell file */
    fprintf (stderr, "Reading %s in %s ...", name, mapset);
    for (row = 0; row < nrows; row++)
    {
	G_percent (row, nrows, 2);
	if (G_get_c_raster_row_nomask(fd, cell, row) < 0)
	    exit(0);
	G_update_cell_stats (cell, ncols, &statf);
    }
/* done */
    G_percent (row, nrows, 2);
    G_close_cell (fd);
    G_free (cell);
    G_rewind_cell_stats (&statf);

    return 0;
}

int 
next_cat (long *x)
{
    long count;
    CELL cat;

    if(G_next_cell_stat (&cat, &count, &statf))
    {
	*x = cat;
	return 1;
    }
    return 0;
}
