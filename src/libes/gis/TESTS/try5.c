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
fprintf (stdout,"read cats\n");
    if (G_read_cats (name, mapset, &cats) < 0)
    {
	fprintf (stdout,"OOPS\n");
	continue;
    }
fprintf (stdout,"ok\n");
/*
    for (cat = 0; cat <= cats.num; cat++)
	fprintf (stdout,"%ld:%s\n", (long)cat, G_get_cat (cat, &cats));
*/
fprintf (stdout,"open cell\n");
    if ((fd = G_open_cell_old (name, mapset)) >= 0)
    {
fprintf (stdout,"reading cell\n");
	for (row = 0; row < nrows; row++)
		G_get_map_row (fd, cell, row);
fprintf (stdout,"close cell\n");
	G_close_cell(fd);
    }
}
}
