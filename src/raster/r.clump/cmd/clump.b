#include "gis.h"
clump(in_fd, out_fd)
{
    CELL *prev_in, *cur_in;
    CELL *prev_clump, *cur_clump;
    CELL *temp_cell, *out_cell;
    CELL *index, *renumber;
    CELL X,UP,LEFT,NEW,OLD;
    CELL label;
    int nrows, ncols;
    int row, col;
    int len, n;
    int pass;

    nrows = G_window_rows();
    ncols = G_window_cols();

/* allocate reclump index */
    index = (CELL *) G_malloc (sizeof (CELL));
    index[0] = 0;

/* allocate CELL buffers one column larger than current window */
    len = (ncols+1) * sizeof (CELL);
    prev_in    = (CELL *) G_malloc (len);
    cur_in     = (CELL *) G_malloc (len);
    prev_clump = (CELL *) G_malloc (len);
    cur_clump  = (CELL *) G_malloc (len);
    out_cell   = (CELL *) G_malloc (len);

/******************************** PASS 1 ************************************
 * first pass thru the input simulates the clump to determine
 * the reclumping index.
 * second pass does the clumping for real
 */
    renumber = 0;
    for (pass = 1; pass <= 2; pass++)
    {
    /* second pass must gnerate a renumbering scheme */
	if (pass == 2)
	{
	    int cur;
	    renumber = (CELL *) G_malloc ((label+1) * sizeof (CELL));
	    cur = 1;
	    for (n = 1; n <= label; n++)
		renumber[index[n] = cur++;
	    renumber[0] = 0;
	}

    /* fake a previous row which is all zero */
	G_zero (prev_in, len);
	G_zero (prev_clump, len);

    /* create a left edge of zero */
	cur_in[0] = 0;
	cur_clump[0] = 0;

    /* initialize clump labels */
	label = 0;

	printf ("CLUMP PASS %d\n", pass);
	for (row = 0; row < nrows; row++)
	{
	    if (G_get_map_row (in_fd, cur_in+1, row) < 0)
		G_fatal_error ("Gclump: error reading cell file");

	    for (col = 1; col <= ncols; col++)
	    {
		X = cur_in[col];
		if (X == 0)            /* don't clump zero data */
		{
		    cur_clump[col] = 0;
		    continue;
		}

		LEFT = cur_in[col-1];
		UP   = prev_in[col];

    /*
     * if the cell value is different above and to the left
     * then we must start a new clump
     * (note: this new clump may eventually collide with another
     *   clump and have to be merged
     */
		if (X == UP && LEFT == 0)
		{
		    cur_clump[col] = prev_clump[col];
		    continue;
		}

		if (X == LEFT && UP == 0)
		{
		    cur_clump[col] = cur_clump[col-1];
		    continue;
		}

		if (X != LEFT && X != UP)        /* start a new clump */
		{
		    label++;
		    cur_clump[col] = label;
		    if (pass == 1)
		    {
			index = (CELL *) G_realloc (index, (label+1) * sizeof(CELL));
			index[label] = label;
		    }
		    continue;
		}
		if (X == LEFT && X != UP)        /* same clump as to the left */
		{
		    cur_clump[col] = cur_clump[col-1];
		    continue;
		}
		if (X == UP && X != LEFT)       /* same clump as above */
		{
		    cur_clump[col] = prev_clump[col];
		    continue;
		}

    /*
     * at this point the cell value X is the same as LEFT and UP
     * so it should go into the same clump. It is possible for
     * the clump value on the left to differ from the clump value
     * above. If this happens we have a conflict and one of the
     * LEFT or UP needs to be reclumped
     */
		if (cur_clump[col-1] == prev_clump[col]) /* ok */
		{
		    cur_clump[col] = prev_clump[col];
		    continue;
		}

    /* conflict! decide which clump to preserve and change the other
     */

		NEW = prev_clump[col];
		OLD = cur_clump[col-1];
		cur_clump[col] = NEW ;
		for(n=0; n<col; n++)
			if (cur_clump[n] == OLD)
				cur_clump[n] = NEW ;

		if (pass == 1)
		    for (n = 1; n <= label; n++)
			if (index[n] == OLD)
			    index[n] = NEW;
	    }

	    if (pass == 2)
	    {
		for (col = 1; col <= ncols; col++)
		    out_cell[col] = index[cur_clump[col]];
		if (G_put_map_row (out_fd, out_cell+1) < 0)
		    G_fatal_error ("Glump: error writing cell file");
	    }

    /* switch the buffers so that the current buffer becomes the previous */
	    temp_cell  = cur_in;
	    cur_in     = prev_in;
	    prev_in    = temp_cell;

	    temp_cell  = cur_clump;
	    cur_clump  = prev_clump;
	    prev_clump = temp_cell;
	}
    }
}
