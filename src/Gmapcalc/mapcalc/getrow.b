#include "gis.h"
getrow(fd, cell, row, col, nrows, ncols)
    register int col, ncols;
    register CELL *cell;
{
    register int i;

    if (row < 0 || row >= nrows)
    {
	while (ncols-- > 0)
	    *cell++ = 0;
	return 1;
    }
    if (G_get_map_row(fd, cell, row) < 0)
	return 0;

/* if column offset, copy cell to itself shifting by col */
    if (col>0)
    {
	for (i = 0; i <ncols-col; i++)
	    cell[i] = cell[i+col];
	while (i < ncols)
	    cell[i++] = 0;
    }
    else if (col < 0)
    {
	col = -col;
	for (i = ncols-1; i >= col; i--)
	    cell[i] = cell[i-col];
	while (i >= 0)
	    cell[i--] = 0;
    }
    return 1;
}
