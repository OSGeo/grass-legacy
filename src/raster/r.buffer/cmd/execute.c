#include "distance.h"
execute_distance( quiet)
int quiet;
{
    int row, col, nrows;
    MAPTYPE *ptr;

	/* find the first 1 in each row, and process that row */

    if ( ! quiet )
       fprintf (stderr, "Finding buffer zones ... ");
    nrows = 0;
    for (row = minrow; row <= maxrow; row++)
    {
	ptr = map + MAPINDEX(row,mincol);
	for (col = mincol; col <= maxcol; col++)
	{
	    if (*ptr++ == 1)
	    {
                if ( ! quiet )
		   G_percent (nrows++, count_rows_with_data, 2);
		process_row (row, col);
		break;
	    }
	}
    }
    if ( ! quiet )
       G_percent (nrows, count_rows_with_data, 2);
}
