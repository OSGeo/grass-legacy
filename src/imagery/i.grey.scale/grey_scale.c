#include "gis.h"
grey_scale(name,mapset)
    char *name;
    char *mapset;
{
    struct Colors colors;
    int fd;
    int nrows, ncols, row;
    CELL *cell;
    struct Cell_stats statf;

    fd = G_open_cell_old (name, mapset);
    if (fd < 0)
    {
	fprintf (stderr, "Unable to read [%s]\n", name);
	return;
    }
    G_init_cell_stats (&statf);
    cell = G_allocate_cell_buf();
    nrows = G_window_rows();
    ncols = G_window_cols();

    fprintf (stderr, "Reading %s ... ", name);
    for (row = 0; row < nrows; row++)
    {
	G_percent (row, nrows, 2);
	if (G_get_map_row(fd, cell, row) < 0)
	    break;
	G_update_cell_stats (cell, ncols, &statf);
    }
    if (row == nrows)
	G_percent (row, nrows, 2);
    free(cell);
    G_close_cell (fd);
    if (row == nrows)
    {
	G_make_histogram_eq_colors (&colors, &statf);
	G_write_colors (name, mapset, &colors);
	G_free_colors(&colors);
	printf ("[%s in %s] now has a grey scale color table\n", name, mapset);
    }
    G_free_cell_stats(&statf);
}
