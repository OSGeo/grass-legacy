#include "gis.h"
main()
{
    char name[30], *mapset;
    char new[30];
    int fd, fd2;
    CELL *cell;
    int nrows, ncols, row, col;

    G_gisinit ("try");

    mapset = G_ask_cell_old ("Enter cell to copy", name);
    if (mapset == NULL) exit(0);
    fd = G_open_cell_old (name, mapset);
    if (fd < 0)
    {
	fprintf (stdout,"OOPS");
	exit(1);
    }
    if (G_ask_cell_new ("",new) == NULL)
	exit(0);
    fprintf (stdout,"opening %s\n", new);
    fd2 = G_open_cell_new (new);
    if (fd2 < 0)
    {
	fprintf (stdout,"OOPS");
	exit(1);
    }
    fprintf (stdout,"opened\n");
    cell = G_allocate_cell_buf();
    nrows = G_window_rows();
    ncols = G_window_cols();
    for (row = 0; row < nrows-1; row++)
    {
fprintf (stdout,"get(%d)\n", row);
	G_get_map_row (fd, cell, row);

fprintf (stdout,"put(%d)\n", row);
	if(G_put_raster_row (fd2, cell, CELL_TYPE) < 0)
		exit(1);

    }
    fprintf (stdout,"closing\n");
    G_close_cell (fd2);
}
