#include "gis.h"

describe (name, mapset, compact, verbose, range, windowed)
    char *name;
    char *mapset;
{
    int fd;
    struct Cell_stats statf;
    CELL *buf, *b;
    int nrows, ncols;
    int row, col;
    struct Cell_head window;
    CELL negmin, negmax, zero, posmin, posmax;
    int G_get_map_row();
    int G_get_map_row_nomask();
    int (*get_row)();

    if (windowed)
    {
	get_row = G_get_map_row;
    }
    else
    {
	char msg[100];
	if (G_get_cellhd (name, mapset, &window) < 0)
	{
	    sprintf (msg, "can't get cell header for [%s] in [%s]",name,mapset);
	    G_fatal_error (msg);
	}
	G_set_window (&window);
	get_row = G_get_map_row_nomask;
    }
    fd = G_open_cell_old (name, mapset);
    if (fd < 0) return 0;

/* allocate the cell buffer */
    buf = G_allocate_cell_buf();


/* start the cell stats */
    if(!range)
    {
	G_init_cell_stats (&statf);
    }
    else
    {
	zero = 0;
	negmin=0;
	negmax=0;
	posmin=0;
	posmax=0;
    }


    nrows = G_window_rows();
    ncols = G_window_cols();

    if (verbose)
	fprintf (stderr,"READING [%s in %s] ...", name,mapset);
    for (row = 0 ; row < nrows; row++)
    {
	if (verbose)
	    G_percent (row, nrows, 2);
	if((*get_row) (fd, b = buf, row) < 0)
	    break;
	if (range)
	{
	    for (col = ncols ;col-- > 0; b++)
	    {
		if (*b == 0)
		    zero = 1;
		else if (*b < 0)
		{
		    if (!negmin)
			negmin = negmax = *b;
		    else if (*b > negmax)
			negmax = *b;
		    else if (*b < negmin)
			negmin = *b;
		}
		else
		{
		    if (!posmin)
			posmin = posmax = *b;
		    else if (*b > posmax)
			posmax = *b;
		    else if (*b < posmin)
			posmin = *b;
		}
	    }
	}
	else
	    G_update_cell_stats (buf, ncols, &statf);
    }
    if (verbose)
	G_percent (nrows, nrows, 2);
    G_close_cell (fd);
    free (buf);

    if (range)
    {
	if (compact)
	    compact_range_list (negmin, negmax, zero, posmin, posmax);
	else
	    range_list (negmin, negmax, zero, posmin, posmax);
    }
    else
    {
	G_rewind_cell_stats (&statf);
	if (compact)
	    compact_list (&statf);
	else
	    long_list (&statf);
	G_free_cell_stats (&statf);
    }
    return 1;
}
