#include "gis.h"

main(argc, argv) char *argv[];
{
    struct History hist;
    struct Categories cats;
    struct Colors colr;
    struct Cell_head cellhd;
    int hist_ok, colr_ok, cats_ok;
    char name[100], *mapset;
    char result[100];
    CELL *cell;
    int nrows, ncols;
    int row, col;
    int infd, outfd;
    int verbose;
    char *me;

    G_gisinit (me = argv[0]);

    verbose = 1;
    if (argc < 2)
	usage (me);
    if (argv[1][0] == '-')
    {
	if (strcmp (argv[1], "-v") == 0)
	    verbose = 0;
	else
	{
	    fprintf (stderr, "%s: %s  illegal option\n", me, argv[1]);
	    usage (me);
	}
	argc--;
	argv++;
    }
    if (argc < 3)
	usage(me);
    strcpy (name, argv[1]);
    strcpy (result, argv[2]);
    mapset = G_find_cell2 (name, "");
    if (mapset == NULL)
    {
	char buf[200];
	sprintf (buf, "cell file [%s] not found", name);
	G_fatal_error (buf);
    }
    if (G_legal_filename (result) < 0)
    {
	char buf[200];
	sprintf (buf, "[%s] illegal name", result);
	G_fatal_error (buf);
    }

    hist_ok = G_read_history (name, mapset, &hist) >= 0;
    colr_ok = G_read_colors (name, mapset, &colr) > 0;
    cats_ok = G_read_cats (name, mapset, &cats) >= 0;

    infd = G_open_cell_old (name, mapset);
    if (infd < 0)
	exit(1);
    if (G_get_cellhd (name, mapset, &cellhd) < 0)
	exit(1);
    cell = G_allocate_cell_buf();
    nrows = G_window_rows();
    ncols = G_window_cols();

    outfd = G_open_cell_new (result);
    if (outfd < 0)
	exit(1);

    if (verbose)
	printf ("percent complete: ");
    for (row = 0; row < nrows; row++)
    {
	if (verbose)
	    percent (row, nrows, 10);
	if (G_get_map_row (infd, cell, row) < 0)
	    exit(1);
	if (G_put_map_row (outfd, cell) < 0)
	    exit(1);
    }
    if (verbose)
	percent (row, nrows, 10);
    G_close_cell (infd);
    if (verbose)
	printf ("Creating support files for %s\n", result);

    G_close_cell (outfd);
    if (cats_ok) G_write_cats (result, &cats);
    if (colr_ok) G_write_colors (result, G_mapset(), &colr);
    if (hist_ok) G_write_history (result, &hist);

    exit(0);
}
