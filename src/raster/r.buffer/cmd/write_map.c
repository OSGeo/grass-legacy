#include <stdlib.h>
#include "distance.h"

    /* write out result */

int write_output_map (char *output, int offset, int quiet)
{
    int fd_in=0, fd_out;
    int row;
    register int col;
    register CELL *cell;
    register MAPTYPE *ptr;
    int k;

    fd_out = G_open_cell_new (output);
    if (fd_out < 0)
    {
	fprintf (stderr, "%s: %s - can't create cell file\n", pgm_name, output);
	exit(1);
    }
    if (offset)
    {
	fd_in = G_open_cell_old (output, G_mapset());
	if (fd_in < 0)
	{
	    fprintf (stderr, "%s: unable to re-open %s\n", pgm_name, output);
	    exit(1);
	}
    }
    cell = G_allocate_cell_buf();
    if ( ! quiet )
       fprintf (stderr, "Writing output map (%s)   ... ", output);

    ptr = map;

    for (row = 0; row < window.rows; row++)
    {
        if ( ! quiet )
           G_percent (row, window.rows, 2);
	col = window.cols;
	if (!offset)
	{
	    while (col-- > 0)
		*cell++ = (CELL) *ptr++ ;
	}
	else
	{
	    if (G_get_map_row_nomask(fd_in, cell, row) < 0)
	    {
		fprintf (stderr, "%s - ERROR re-reading %s\n", pgm_name, output);
		exit(1);
	    }
	    while (col-- > 0)
	    {
		if (*cell == 0 && *ptr != 0)
		    *cell = (CELL) *ptr + offset ;
		cell++;
		ptr++;
	    }
	}
	cell -= window.cols;
	/* set 0 to NULL */
	for (k=0; k < window.cols; k++)
	       if (cell[k] == 0) G_set_null_value(&cell[k], 1, CELL_TYPE);
	if (G_put_raster_row (fd_out, cell, CELL_TYPE) < 0)
	{
	    fprintf (stderr, "%s - ERROR writing %s\n", pgm_name, output);
	    exit(1);
	}
    }
    if ( ! quiet )
       G_percent (row, window.rows, 2);
    G_free(cell);

    if (offset)
	G_close_cell(fd_in);

    G_close_cell(fd_out);

    return 0;
}
