#include "global.h"
rectify (name, mapset, result,order)
    char *name;
    char *mapset;
    char *result;
    int order;
{
    struct Cell_head cellhd, win;
    int ncols, nrows;
    int row, col;
    int infd, outfd;
    CELL *cell;

    select_current_env();
    if (G_get_cellhd (name, mapset, &cellhd) < 0)
	return 0;

/* open the result file into target window
 * this open must be first since we change the window later
 * cell files open for writing are not affected by window changes
 * but those open for reading are
 *
 * also tell open that cell file will have the same format
 * (ie number of bytes per cell) as the file being rectified
 */

    select_target_env();
    G_set_window (&target_window);
    G_set_cell_format (cellhd.format);
    outfd = G_open_cell_new_random (result);
    select_current_env();
    if (outfd < 0)
	return 0;


/* open the file to be rectified
 * set window to cellhd first to be able to read file exactly
 */
    G_set_window (&cellhd);
    infd = G_open_cell_old (name, mapset);
    if (infd < 0)
    {
	close (infd);
	return 0;
    }
    cell = (CELL *) G_calloc (G_window_cols()+1, sizeof(CELL));
    *cell = 0;

    G_copy (&win, &target_window, sizeof(win));

    win.west += win.ew_res/2;
    ncols = target_window.cols;
    col = 0;

    while (ncols > 0)
    {
	if ((win.cols = ncols) > NCOLS)
	    win.cols = NCOLS;
	win.north = target_window.north - win.ns_res/2;
	nrows = target_window.rows;
	row = 0;

	while (nrows > 0)
	{
	    if ((win.rows = nrows) > NROWS)
		win.rows = NROWS;

	    compute_georef_matrix (&cellhd, &win,order);
	    perform_georef (infd, cell);
	    write_matrix (outfd, row, col);

	    nrows -= win.rows;
	    row += win.rows;
	    win.north -= (win.ns_res * win.rows);
	}

	ncols -= win.cols;
	col += win.cols;
	win.west += (win.ew_res * win.cols);
    }

    select_target_env();
    G_close_cell (outfd);
    select_current_env();

    G_close_cell (infd);
    free (cell);

    return 1;
}
