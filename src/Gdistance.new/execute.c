#include "distance.h"
execute_distance()
{
    int row, col, nrows;
    MAPTYPE *ptr;

	/* find the first 1 in each row, and process that row */

    fprintf (stderr, "Finding distance zones ... ");
    nrows = 0;
    for (row = minrow; row <= maxrow; row++)
    {
	ptr = map + MAPINDEX(row,mincol);
	for (col = mincol; col <= maxcol; col++)
	{
	    if (*ptr++ == 1)
	    {
		G_percent (nrows++, count_rows_with_data, 2);
		process_row (row, col);
		break;
	    }
	}
    }
    G_percent (nrows, count_rows_with_data, 2);
}
