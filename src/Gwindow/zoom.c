#include "gis.h"
zoom (window, name, mapset)
    struct Cell_head *window;
{
    int fd;
    CELL *cell, *c;
    int row, col;
    int nrows, ncols;
    int top,bottom,left,right,mark;
    double north, south, east, west;

    adjust_window (window);
    G_set_window (window);
    nrows = window->rows;
    ncols = window->cols;

    fd = G_open_cell_old (name, mapset);
    if (fd < 0)
    {
	char msg[100];

	sprintf (msg, "Unable to open cell file <%s> in <%s>",
		name, mapset);
	G_fatal_error (msg);
    }
    cell = G_allocate_cell_buf();

/* find first non-zero row */
    top = nrows;
    bottom = -1;
    left = ncols;
    right = -1;
    for (row = 0; row < nrows; row++)
    {
	if (G_get_map_row (fd, c = cell, row) < 0)
	    exit(1);
	for (col = 0; col < ncols; col++)
	    if (*c++)
		break;
	if (col == ncols)
	    continue;
	if (row < top) top = row;
	if (row > bottom) bottom = row;
	if (col < left) left = col;
	for (mark = col++; col < ncols; col++)
	    if (*c++)
		mark = col;
	if (mark > right)
	    right = mark;
    }
    G_close_cell (fd);
    free (cell);

/* no data everywhere? */
    if (bottom < 0)
	return 0;
    
    north = window->north - top * window->ns_res;
    south = window->north - (bottom+1) * window->ns_res;
    west = window->west + left * window->ew_res;
    east = window->west + (right+1) * window->ew_res;

    window->north = north;
    window->south = south;
    window->east  = east ;
    window->west  = west ;

    return 1;
}
