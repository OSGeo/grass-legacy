#include "gis.h"
main (argc, argv) char *argv[];
{
    char name[30], *mapset;
    int fd;
    CELL *cell;
    int nrows, ncols, row, col;
    CELL cat;
    long count;
    struct Cell_stats statf, *s;

    if (argc != 2)
    {
	printf ("Usage: %s cellfile\n", argv[0]);
	exit(1);
    }
    G_gisinit ("try");

    s = &statf;
    printf ("sizeof counts %ld\n", sizeof (s->node[0].count));
    cell = G_allocate_cell_buf();
    nrows = G_window_rows();
    ncols = G_window_cols();
    mapset = G_find_cell (argv[1], "");
    if (mapset == NULL)
    {
	printf ("%s not found\n", argv[1]);
	exit(1);
    }
    fd = G_open_cell_old (argv[1], mapset);
    if (fd < 0)
	exit(1);
    G_init_cell_stats (&statf);
    for (row = 0; row < nrows; row++)
    {
	G_get_map_row (fd, cell, row);
	G_update_cell_stats (cell, ncols, &statf);
    }
    G_close_cell(fd);
    G_rewind_cell_stats (&statf);
    while (G_next_cell_stat (&cat, &count, &statf))
	printf ("%ld:%ld ",(long)cat, count);
    printf ("\n");
    G_free_cell_stats(&statf);
}
