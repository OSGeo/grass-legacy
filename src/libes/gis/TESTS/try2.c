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
	printf ("OOPS");
	exit(1);
    }
    if (G_ask_cell_new ("",new) == NULL)
	exit(0);
    printf ("opening %s\n", new);
    fd2 = G_open_cell_new (new);
    if (fd2 < 0)
    {
	printf ("OOPS");
	exit(1);
    }
    printf ("opened\n");
    cell = G_allocate_cell_buf();
    nrows = G_window_rows();
    ncols = G_window_cols();
    for (row = 0; row < nrows-1; row++)
    {
printf ("get(%d)\n", row);
	G_get_map_row (fd, cell, row);

printf ("put(%d)\n", row);
	if(G_put_map_row (fd2, cell) < 0)
		exit(1);

    }
    printf ("closing\n");
    G_close_cell (fd2);
}
