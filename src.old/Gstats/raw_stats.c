#include "global.h"

raw_stats (fd, verbose, non_zero, rowcol)
    int fd[];
{
    CELL *cell[NFILES];
    register int i;
    int row, col;

/* allocate i/o buffers for each cell file */
    for (i = 0; i < nfiles; i++)
	cell[i] = G_allocate_cell_buf();

/* here we go */
    if (verbose)
	fprintf (stderr, "%s:  complete ... ", G_program_name());
    for (row = 0; row < nrows; row++)
    {
	if (verbose)
	    percent (row, nrows, 5);
	for (i = 0; i < nfiles; i++)
	    if (get_row (fd[i], cell[i], row) < 0)
		exit(1);
	if (maskfd >= 0)
	    G_get_map_row_nomask (maskfd, mask, row);

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
	    if (rowcol)
		printf ("%d:%d:", row, col);
	    for (i = 0; i < nfiles; i++)
		printf ("%s%ld",i?":":"", (long)cell[i][col]);
	    printf ("\n");
	}
    }
    if (verbose)
	percent (nrows, nrows, 5);
}
