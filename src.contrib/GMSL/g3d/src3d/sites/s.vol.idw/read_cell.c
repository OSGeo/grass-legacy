#include "gis.h"

read_cell (name)
    char *name;
{
    int fd;
    CELL *cell;
    struct Cell_head window, cellhd;
    int row, col;
    double z,north;
    char *mapset;

    mapset = G_find_cell (name,"");
    if (mapset == NULL)
    {
	fprintf (stderr, "%s: %s - map not found\n", G_program_name(), name);
	exit(1);
    }

    G_get_window (&window);

/* Set window to align with input cell file */
    G_get_cellhd (name, mapset, &cellhd);
    G_align_window (&window, &cellhd);
    G_set_window (&window);

    cell = G_allocate_cell_buf();

    fd = G_open_cell_old (name, mapset);
    if (fd < 0)
    {
	fprintf (stderr, "%s: can't open %s\n", G_program_name(), name);
	exit(1);
    }

    fprintf (stderr, "Reading raster map (%s) ...", name);

    north = window.north - window.ns_res/2.0;
    for (row = 0; row < window.rows; row++)
    {
	G_percent (row, window.rows, 1);
	north += window.ns_res;
	if (G_get_map_row_nomask (fd, cell, row) < 0)
	    exit(1);
	for (col = 0; col < window.cols; col++)
	    if (z = cell[col])
		newpoint(z,window.west+(col+.5)*window.ew_res,north);
    }
    G_percent (row, window.rows, 1);

    G_close_cell (fd);
    free(cell);

/* reset the window */
    G_get_window (&window);
    G_set_window (&window);
}
