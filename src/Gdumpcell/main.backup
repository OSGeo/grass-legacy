/* %W% %G% */
#include "gis.h"
main(argc,argv) char *argv[];
{
    CELL *cell;
    char *name, *mapset;
    int cf;
    int row,col;
    int nrows, ncols;
    int i;
    int have_cell;
    int n;
    char fmt[20];

    have_cell = 0;
    strcpy (fmt, "%ld ");

    for (i = 1; i < argc; i++)
    {
	if (sscanf (argv[i], "-%d", &n) == 1)
	{
	    if (n > 0)
		sprintf (fmt, "%%%dld ", n);
	    else
		sprintf (fmt, "%%%dld", n);
	}
	else
	{
	    name = argv[i];
	    have_cell++;
	}
    }
    if (have_cell != 1)
    {
	fprintf (stderr, "Usage: %s [-#] cellfile\n", argv[0]);
	exit(1);
    }

    G_gisinit (argv[0]);

    mapset = G_find_cell (name,"");
    if (mapset == NULL)
    {
	fprintf (stderr, "%s: %s: cellfile not found\n", argv[0], name);
	exit(1);
    }

    cf = G_open_cell_old (name, mapset);
    if (cf < 0)
	exit(1);

    cell = G_allocate_cell_buf();

    nrows = G_window_rows();
    ncols = G_window_cols();

    for (row = 0; row < nrows; row++)
    {
	if (G_get_map_row (cf, cell, row) < 0)
	    exit(1);
	for (col = 0; col < ncols; col++)
	    printf (fmt, (long)cell[col]);
	printf ("\n");
    }
    exit(0);
}
