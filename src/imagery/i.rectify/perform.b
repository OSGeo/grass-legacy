#include "global.h"

perform_georef (infd, outfd, cell, supercell)
    CELL *cell;
    SUPERCELL *supercell;
{
    int row;
    int curidx;
    int idx;
    int i;

    if (cell == NULL)
    {
	i = sizeof (SUPERCELL) * matrix_cols;
	for (row = 0; row < matrix_rows; row++)
	    G_zero (supercell_buf[row], i);
    }
    else
    {
	i = sizeof (CELL) * matrix_cols;
	for (row = 0; row < matrix_rows; row++)
	    G_zero (cell_buf[row], i);
    }

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
/*
printf (" curidx %d\n", curidx);
*/
	if (curidx >= matrix_rows)
		break;
/*
printf ("read row %d\n", row);
*/

	if (cell == NULL)
	{
	    if (G_get_super_map_row_nomask (infd, supercell+1, row) < 0)
		return 0;
	}
	else if (G_get_map_row_nomask (infd, cell+1, row) < 0)
		return 0;

	for (i=curidx; i < matrix_rows; i++)
	{
	    idx = row_idx[i];
	    if (row != row_min[idx])
		break;
/*
printf ("  process matrix row %d\n", idx);
*/
	    if (cell == NULL)
		do_supercell (row, row_map[idx], col_map[idx], supercell+1, supercell_buf[idx]);
	    else
		do_cell (row, row_map[idx], col_map[idx], cell+1, cell_buf[idx]);

	    row_min[idx]++;
	    if (row_min[idx] > row_max[idx])
		row_min[idx] = -1;
	}

    }
}

static
do_cell (row, rmap, cmap, in, out)
    ROWCOL *rmap, *cmap;
    CELL *in, *out;
{
    int col;

    for (col = 0; col < matrix_cols; col++)
    {
	if (row == *rmap++)
	    *out = in[*cmap];
	out++;
	cmap++;
    }
}

static
do_supercell (row, rmap, cmap, in, out)
    ROWCOL *rmap, *cmap;
    SUPERCELL *in, *out;
{
    int col;

    for (col = 0; col < matrix_cols; col++)
    {
	if (row == *rmap++)
	    *out = in[*cmap];
	out++;
	cmap++;
    }
}
