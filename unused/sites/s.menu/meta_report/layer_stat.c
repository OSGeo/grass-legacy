#include "gis.h"
#include "site.h"

int layer_stats (FILE *meta_report, char *layer, char *mapset,
    int layer_num, int with_stats)
{
    struct Categories cats;
    struct Cell_stats statf;
    struct Range range;
    int fd;
    CELL *cell;
    int row;
    int nrows;
    int ncols;
    CELL n;
    long count;

    fprintf (stdout,"%-15s ... ", layer); fflush (stdout);


    if (G_read_cats (layer, mapset, &cats) == -1)
    {
	fprintf (stdout,"** error - category information missing\n");
	return -1;
    }

    if (with_stats)
    {
	if ((fd = G_open_cell_old (layer, mapset)) < 0)
	{
	    G_free_cats (&cats);
	    fprintf (stdout,"** error - cant open raster file **\n");
	    return -1;
	}

	G_init_cell_stats (&statf);
	cell = G_allocate_cell_buf () ;

	counter_reset ("0   percent complete", 1);

	nrows = G_window_rows ();
	ncols = G_window_cols ();

	for (row = 0; row < nrows; row++)
	{
	    if(G_get_map_row (fd, cell, row) < 0)
	    {
		fprintf (stdout,"** error - can't read raster file **\n");
		G_free (cell);
		G_close_cell (fd);
		G_free_cell_stats (&statf);
		G_free_cats (&cats);
		return -1;
	    }

	    G_update_cell_stats (cell, ncols, &statf);

	    counter ( (row+1) * 100 / nrows );
	}
	G_close_cell (fd);
	G_free (cell);
	fprintf (stdout,"\n");
	G_rewind_cell_stats (&statf);
	while (G_next_cell_stat (&n, &count, &statf))
	    fprintf(meta_report,"cat|%d|%ld|%ld|%s\n",
		layer_num, (long)n, count, G_get_cat (n, &cats));
	G_free_cell_stats (&statf);
    }
    else
    {
	count = 0;
	G_read_range (layer, mapset, &range);

	for (n = range.min; n <= range.max; n++)
	    fprintf(meta_report,"cat|%d|%ld|%ld|%s\n",
		layer_num, (long)n, count, G_get_cat (n, &cats));
    }
    G_free_cats (&cats);

    return 0;
}
