#include "gis.h"
main()
{
    char name[30], *mapset;
    int fd;
    CELL *cell;
    int nrows, ncols, row, col;

    /*
    G_gisinit ("try");
    */

    mapset = G_ask_cell_old ("Enter cell to dump", name);
    if (mapset == NULL) exit(0);
    fd = G_open_cell_old (name, mapset);
    if (fd < 0)
    {
	fprintf (stdout,"OOPS");
	exit(1);
    }
    cell = G_allocate_cell_buf();
    nrows = G_window_rows();
    ncols = G_window_cols();
    for (row = 0; row < nrows; row++)
    {
	fprintf (stdout,"%3d: ", row);
	G_get_map_row (fd, cell, row);
	for (col = 0; col < ncols; col++)
		fprintf (stdout,"%ld ", (long) cell[col]);
	fprintf (stdout,"\n>");
	/*
	G_gets(cell);
	*/
    }
}
