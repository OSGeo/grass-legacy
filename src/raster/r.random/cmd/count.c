#include "gis.h"

long
get_count (name, mapset, cat_zero, zero, verbose)
    char *name, *mapset;
    CELL *cat_zero;
{
    long count;
    int nrows, ncols, row, col;
    int fd;
    CELL *cell;
    char msg[200];


    nrows = G_window_rows();
    ncols = G_window_cols();
    cell = G_allocate_cell_buf();

    /* open the input file */
    fd = G_open_cell_old (name, mapset);
    if (fd < 0)
    {
	sprintf (msg, "%s: unable to open raster file [%s]", G_program_name(), name);
	G_fatal_error (msg);
	exit(1);
    }

    count = 0;
    if (verbose)
	fprintf (stderr, "Reading [%s] ... ", name);
    for (row = 0; row < nrows; row++)
    {
	if (verbose)
	    G_percent (row, nrows, 2);
	if (G_get_map_row (fd, cell, row) < 0)
	{
	    sprintf (msg, "%s: can't read raster file [%s]", G_program_name(), name);
	    G_fatal_error (msg);
	    exit(1);
	}
	col = ncols;
	if (zero)
	{
	    while (col-- > 0)
	    {
		if (*cell >= *cat_zero)
		    *cat_zero = *cell + 1;
		count++;
		cell++;
	    }
	}
	else
	{
	    while (col-- > 0)
	    {
		if (*cell++)
		    count++;
	    }
	}
	cell -= ncols;
    }
    if (zero && *cat_zero == 0)
	*cat_zero = 1;
    if (verbose)
	G_percent (nrows, nrows, 2);
    G_close_cell(fd);
    free(cell);
    return count;
}
