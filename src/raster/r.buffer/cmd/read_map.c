#include "distance.h"

    /* read the input map. convert non-nulls to 1 */

int read_input_map (char *input, char *mapset, int quiet, int ZEROFLAG)
{
    int fd;
    int row;
    int hit;
    register int col;
    register CELL *cell;
    register MAPTYPE *ptr;

    map = (MAPTYPE *) G_malloc (window.rows * window.cols * sizeof(MAPTYPE));

    fd = G_open_cell_old (input, mapset);
    if (fd < 0)
    {
	fprintf (stderr, "%s: %s - can't open\n", pgm_name, input);
	exit(1);
    }
    cell = G_allocate_cell_buf();

    ptr = map;

    minrow = -1; maxrow = -1;
    mincol = window.cols; maxcol = 0;

    if ( ! quiet )
       fprintf (stderr, "Reading input map (%s)    ... ", input);

    count_rows_with_data = 0;

    for (row = 0; row < window.rows; row++)
    {
	hit = 0;
        if ( ! quiet )
	   G_percent (row, window.rows, 2);

	if (G_get_c_raster_row (fd, cell, row) < 0)
	{
	    fprintf (stderr, "%s - ERROR reading %s\n", pgm_name, input);
	    exit(1);
	}
	for (col = 0; col < window.cols; col++)
	{
	    if (ZEROFLAG)
	    {
	      if(*ptr++ = (*cell++ != 0))
		    {
			if (minrow < 0) minrow = row;
			maxrow = row;
			if (col < mincol) mincol = col;
			if (col > maxcol) maxcol = col;
			if (!hit)
			{
			    count_rows_with_data++;
			    hit = 1;
			}
	      }
	    }
	    else /* use NULL */
	    {
	      if(*ptr++ = !G_is_c_null_value(cell++))
		    {
			if (minrow < 0) minrow = row;
			maxrow = row;
			if (col < mincol) mincol = col;
			if (col > maxcol) maxcol = col;
			if (!hit)
			{
			    count_rows_with_data++;
			    hit = 1;
			}
	      }
	    }
	}
	cell -= window.cols;
    }
    if ( ! quiet )
       G_percent (row, window.rows, 2);
    G_close_cell(fd);
    G_free (cell);

    return 0;
}
