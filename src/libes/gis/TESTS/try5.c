#include "gis.h"

main()
{
    char name[30], *mapset;
    char *G_get_cat();
    struct Categories cats;
    CELL *cell;
    int row;
    int nrows;
    CELL cat;
    int fd;

    G_gisinit("try");
    cell = G_allocate_cell_buf();
    nrows = G_window_rows();
    while (mapset = G_ask_cell_old ("", name))
    {
printf ("read cats\n");
    if (G_read_cats (name, mapset, &cats) < 0)
    {
	printf ("OOPS\n");
	continue;
    }
printf ("ok\n");
/*
    for (cat = 0; cat <= cats.num; cat++)
	printf ("%ld:%s\n", (long)cat, G_get_cat (cat, &cats));
*/
printf ("open cell\n");
    if ((fd = G_open_cell_old (name, mapset)) >= 0)
    {
printf ("reading cell\n");
	for (row = 0; row < nrows; row++)
		G_get_map_row (fd, cell, row);
printf ("close cell\n");
	G_close_cell(fd);
    }
}
}
