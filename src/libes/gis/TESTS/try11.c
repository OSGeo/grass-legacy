#include "gis.h"
main()
{
    int fd;int n;
    CELL *cell;
    char *mapset, name[100];
    int row, col, nrows, ncols;

    G_gisinit ("try");
    cell = G_allocate_cell_buf();
    nrows = G_window_rows();
    ncols = G_window_cols();
    mapset = G_ask_cell_old ("enter x", name);
    if (!mapset) exit(0);
    for (n = 0; n < 2; n++)
    {
	printf ("step %d\n", n+1);
	fd = G_open_cell_old (name, mapset);
	for (row = 0; row < nrows; row++)
	{
	    G_get_map_row (fd, cell, row);
	    for (col = 0; col < ncols; col++)
		if (cell[col] != 1000)
		    printf ("row %d, col %d, value %ld\n", row, col, (long)cell[col]);
	}
	G_close_cell (fd);
    }
}
