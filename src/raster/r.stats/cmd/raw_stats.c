#include "global.h"

raw_stats (fd, verbose, non_zero, with_coordinates, with_xy, with_labels)
    int fd[];
{
    CELL **cell;
    register int i;
    int row, col;
    double G_row_to_northing(), G_col_to_easting();
    struct Cell_head window;
    char nbuf[100], ebuf[100];

/* allocate i/o buffers for each cell file */
    cell = (CELL **) G_calloc (nfiles, sizeof (CELL *));
    for (i = 0; i < nfiles; i++)
	cell[i] = G_allocate_cell_buf();

/* get window */
    if (with_coordinates)
	G_get_set_window (&window);

/* here we go */
    if (verbose)
	fprintf (stderr, "%s: ", G_program_name());

    for (row = 0; row < nrows; row++)
    {
	if (verbose)
	    G_percent (row, nrows, 2);
	for (i = 0; i < nfiles; i++)
	    if (get_row (fd[i], cell[i], row) < 0)
		exit(1);
	if (maskfd >= 0)
	    G_get_map_row_nomask (maskfd, mask, row);

	if (with_coordinates)
	    G_format_northing (G_row_to_northing(row+.5, &window), nbuf, -1);

	for (col = 0; col < ncols; col++)
	{
	    if (!mask[col]) continue;
	    if (non_zero)
	    {
		for (i = 0; i < nfiles; i++)
		    if (cell[i][col])
			break;
		if (i == nfiles)
		    continue;
	    }
	    if (with_coordinates)
	    {
		G_format_easting (G_col_to_easting(col+.5, &window), ebuf, -1);
		printf ("%s%s%s%s", ebuf,fs,nbuf,fs);
	    }
	    if (with_xy)
		printf ("%d%s%d%s", col+1,fs,row+1,fs);
	    printf ("%ld", (long)cell[0][col]);
	    if (with_labels)
		printf ("%s%s", fs, G_get_cat (cell[0][col], &labels[0]));
	    for (i = 1; i < nfiles; i++)
	    {
		printf ("%s%ld", fs, (long)cell[i][col]);
		if (with_labels)
		    printf ("%s%s", fs, G_get_cat (cell[i][col], &labels[i]));
	    }
	    printf ("\n");
	}
    }
    if (verbose)
	G_percent (nrows, nrows, 2);
}
