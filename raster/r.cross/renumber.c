#include <stdlib.h>
#include "glob.h"
#include "local_proto.h"

int renumber (int in, int out, int verbose)
{
    CELL *cell, *c;
    int row, col;

    cell = G_allocate_cell_buf ();

    if (verbose)
	fprintf (stderr, "%s: STEP 3 ... ", G_program_name());
    for (row = 0; row < nrows; row++)
    {	
	if (verbose)
	    G_percent (row, nrows, 5);
	if (G_get_map_row (in, c = cell, row) < 0)
	    exit(1);
	col = ncols;
	while (col-->0)
	{
	    *c = table[*c];
	    c++;
	}
	if (G_put_raster_row (out, cell, CELL_TYPE) < 0)
	    exit(1);
    }
    if (verbose)
	G_percent (row, nrows, 10);
    G_free(cell);

    return 0;
}
