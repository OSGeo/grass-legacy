#include "global.h"

static ROWCOL *rmap, *cmap, left, right;

perform_georef (infd, cell)
    CELL *cell;
{
    int row;
    int curidx;
    int idx;
    int i;

    i = sizeof (CELL) * matrix_cols;
    for (row = 0; row < matrix_rows; row++)
	G_zero (cell_buf[row], i);

    curidx = 0;
    while (1)
    {
/* find first row */
	while (curidx < matrix_rows)
	{
	    idx = row_idx[curidx];
	    row = row_min[idx];
	    if (row >= 0)
		break;
	    curidx++;
	}
/***
fprintf (Bugsr," curidx %d\n", curidx);
***/
	if (curidx >= matrix_rows)
		break;
/***
fprintf (Bugsr,"read row %d\n", row);
***/

	if (G_get_map_row_nomask (infd, cell+1, row) < 0)
		return 0;

	for (i=curidx; i < matrix_rows; i++)
	{
	    idx = row_idx[i];
	    if (row != row_min[idx])
		break;
/***
fprintf (Bugsr,"  process matrix row %d\n", idx);
***/
	    rmap = row_map[idx];
	    cmap = col_map[idx];
	    left = row_left[idx];
	    right = row_right[idx];
	    do_cell (row, cell+1, cell_buf[idx]);

	    row_min[idx]++;
	    if (row_min[idx] > row_max[idx])
		row_min[idx] = -1;
	    row_left[idx] = left;
	    row_right[idx] = right;
	}
    }
}

static
do_cell (row, in, out)
    CELL *in, *out;
{
    int col;

    for ( ; left <= right; left++)
    {
	if (rmap[left] < 0) continue;
	if (rmap[left] != row) break;
	out[left] = in[cmap[left]];
    }
    for ( ; left <= right; right--)
    {
	if (rmap[right] < 0) continue;
	if (rmap[right] != row) break;
	out[right] = in[cmap[right]];
    }

    for (col = left; col <= right; col++)
    {
	if (rmap[col] == row)
	    out[col] = in[cmap[col]];
    }
}


