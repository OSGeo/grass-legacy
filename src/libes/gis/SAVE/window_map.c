/* %W% %G% */
/*************************************************************
 * G__create_window_mapping(fd)
 *    int fd                   file descriptor for map to be mapped
 *
 * function:
 *     create mapping from cell header into window
 *     the boundaries and resolution of the two spaces do not
 *     have to be the same or aligned in any way.
 * parms:
 *     fd:      open file descriptor for cell file
 *
 * called by:
 *     G_open_cell_old()
 *     G_set_window()
 ***************************************************************
 * G_map_window_col (src, dst, col)
 *   struct Cell_head *src:      source window (struct Cell_head *)
 *   struct Cell_head *dst:      destination window (struct Cell_head *)
 *   int col:                    destination window col to convert
 *
 * function:
 *     convert destination window col to a src window col
 *
 *
 * called by:
 *     G__create_window_mapping()
 *     user programs as needed
 ***************************************************************
 * G_map_window_row (src, dst, row)
 *   struct Cell_head *src:      source window (struct Cell_head *)
 *   struct Cell_head *dst:      destination window (struct Cell_head *)
 *   int row:                    destination window row to convert
 *
 * function:
 *     convert destination window row to a src window row
 *
 * called by:
 *     G_get_map_row ()
 *     user programs as needed
 ***************************************************************
 * G_window_rows(), G_window_cols()
 *
 * function
 *      return the number of rows, cols in the current window
 *
 * parms: (none)
 *
 * called by
 *      any user program
 *************************************************************/

#include "G.h"

#define FCB     G__.fileinfo[fd]
#define CMAP    FCB.col_map
#define SRC     FCB.cellhd
#define DST     G__.window
#define WINDOW_NCOLS  DST.cols
#define WINDOW_NROWS  DST.rows

G__create_window_mapping (fd)
{
    short *col;
    int i;
    int x;
    double C1, C2;

    G__init_window () ;

#define alloc_index(n) (short *) G_malloc((n)*sizeof(short))

    if (FCB.open_mode >= 0 && FCB.open_mode != OPEN_OLD)  /* open for write? */
        return;
    if (FCB.open_mode == OPEN_OLD) /* already open ? */
        free (CMAP);

    col = CMAP = alloc_index (WINDOW_NCOLS) ;

/*
 * for each column in the window, go to center of the cell,
 * compute nearest column in the data file
 * if column is not in data file, set column to -1
 */
    C1 = DST.ew_res / SRC.ew_res ;
    C2 = (DST.west - SRC.west + DST.ew_res/2.0) / SRC.ew_res; 
    for (i = 0; i < WINDOW_NCOLS; i++)
    {
	x = C2;
	if (C2 < x)    /* adjust for rounding of negatives */
	    x--;
	if (x < 0 || x >= SRC.cols) /* not in data file */
	    x = -1;
	*col++ = x;
	C2 += C1;
    }
#ifdef DEBUG
printf ("create window mapping (%d cols)", WINDOW_NCOLS);
    for (i = 0; i < WINDOW_NCOLS; i++)
	printf ("%s%ld", i%15?" ":"\n", (long)CMAP[i]);
    printf ("\n");
#endif

/*
 * compute C1,C2 for row window mapping 
 */
    FCB.C1 = DST.ns_res / SRC.ns_res ;
    FCB.C2 = (SRC.north - DST.north + DST.ns_res/2.0) / SRC.ns_res; 
}

/* convert row of dst window to row of src window */
G_map_window_row (src, dst, row)
    struct Cell_head *src, *dst;
{
    double r;
    int x;


/* check bounds of dst row */
    if (row < 0 || row >= dst->rows)
        return -1;

/* find (float) row based on utm of dst cell center */
    r = (src->north + dst->ns_res * (row + .5) - dst->north)/src->ns_res;
    x = r;

/* adjust for rounding up of negative values */
    if (r < x)
        x--;

    return (x < 0 || x >= src->rows) ? -1 : x ;
}

G_map_window_col (src, dst, col)

    struct Cell_head *src, *dst;
{
    double c;
    int x;

/* check bounds of dst col */
    if (col < 0 || col >= dst->cols)
        return -1;

/* find (float) col based on utm of dst cell center */
    c = (dst->west + dst->ew_res * (col + .5) - src->west)/src->ew_res;
    x = c;

/* adjust for rounding up of negative values */
    if (c < x)
        x--;

    return (x < 0 || x >= src->cols) ? -1 : x ;
}

G_window_rows ()
{
    G__init_window () ;
    return WINDOW_NROWS;
}

G_window_cols ()
{
    G__init_window () ;
    return WINDOW_NCOLS;
}

G__init_window ()
{
    if (!G__.window_set)
    {
        G__.window_set = 1;
        G_get_window (&G__.window);
    }
}

/* this routine works fine if the mask is not set
 * may give incorrect results with a mask, since the
 * mask row may have a different repeat value
 * can be fixed by doing it for the mask as well and using
 * the smaller value
 */

G_row_repeat_nomask (fd, row)
{
    double f;
    int r1, r2;
    int count;

    count = 1;

/* r1 is the row in the cell file itself.
 * r2 is the next row(s) in the cell file
 * see get_row.c for details on this calculation
 */
    f = row * FCB.C1 + FCB.C2;
    r1 = f;
    if (f < r1)
	r1--;

    while (++row < WINDOW_NROWS)
    {
	f = row * FCB.C1 + FCB.C2;
	r2 = f;
	if (f < r2)
	    r2--;
	if (r1 != r2)
	    break;
	count++;
    }
    return count;
}
