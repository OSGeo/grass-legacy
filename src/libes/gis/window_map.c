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

#include <stdlib.h>
#include "gis.h"
#include "G.h"

#define FCB     G__.fileinfo[fd]
#define CMAP    FCB.col_map
#define CELLHD  FCB.cellhd
#define WINDOW  G__.window
#define WINDOW_NCOLS  WINDOW.cols
#define WINDOW_NROWS  WINDOW.rows

int G__create_window_mapping (int fd)
{
    COLUMN_MAPPING *col;
    int i;
    int x;
    double C1, C2;
    double west;

    G__init_window () ;

#define alloc_index(n) (COLUMN_MAPPING *) G_malloc((n)*sizeof(COLUMN_MAPPING))

    if (FCB.open_mode >= 0 && FCB.open_mode != OPEN_OLD)  /* open for write? */
        return 0;
    if (FCB.open_mode == OPEN_OLD) /* already open ? */
        free (CMAP);

    col = CMAP = alloc_index (WINDOW_NCOLS) ;

/*
 * for each column in the window, go to center of the cell,
 * compute nearest column in the data file
 * if column is not in data file, set column to 0
 *
 * for lat/lon move window so that west is bigger than
 * cellhd west.
 */
    west = WINDOW.west;
    if (WINDOW.proj == PROJECTION_LL)
    {
	while (west > CELLHD.west + 360.0)
	    west -=360.0;
	while (west < CELLHD.west)
	    west += 360.0;
    }

    C1 = WINDOW.ew_res / CELLHD.ew_res ;
    C2 = (west - CELLHD.west + WINDOW.ew_res/2.0) / CELLHD.ew_res; 
    for (i = 0; i < WINDOW_NCOLS; i++)
    {
	x = C2;
	if (C2 < x)    /* adjust for rounding of negatives */
	    x--;
	if (x < 0 || x >= CELLHD.cols) /* not in data file */
	    x = -1;
	*col++ = x+1;
	C2 += C1;
    }

/* do wrap around for lat/lon */
    if (WINDOW.proj == PROJECTION_LL)
    {
	col = CMAP;
	C2 = (west - 360.0 - CELLHD.west + WINDOW.ew_res/2.0) / CELLHD.ew_res; 
	for (i = 0; i < WINDOW_NCOLS; i++)
	{
	    x = C2;
	    if (C2 < x)    /* adjust for rounding of negatives */
		x--;
	    if (x < 0 || x >= CELLHD.cols) /* not in data file */
		x = -1;
	    if (*col == 0)  /* only change those not already set */
		*col = x+1;
	    col++;
	    C2 += C1;
	}
    }

#ifdef DEBUG
fprintf (stderr, "create window mapping (%d cols)", WINDOW_NCOLS);
    for (i = 0; i < WINDOW_NCOLS; i++)
	fprintf (stderr, "%s%ld", i%15?" ":"\n", (long)CMAP[i]);
    fprintf (stderr, "\n");
#endif

/*
 * compute C1,C2 for row window mapping 
 */
    FCB.C1 = WINDOW.ns_res / CELLHD.ns_res ;
    FCB.C2 = (CELLHD.north - WINDOW.north + WINDOW.ns_res/2.0) / CELLHD.ns_res; 

    return 0;
}

double G_northing_to_row (double north,
    struct Cell_head *window)
{
    return (window->north - north) / window->ns_res;
}

double G_adjust_east_longitude (
    double east,double west)
{
    while (east > west + 360.0)
	east -=360.0;
    while (east <= west)
	east += 360.0;
    return east;
}

double G_adjust_easting ( double east,
    struct Cell_head *window)
{
    if (window->proj == PROJECTION_LL)
    {
	east = G_adjust_east_longitude(east, window->west);
	if (east > window->east && east == window->west + 360)
	    east = window->west;
    }
    return east;
}

double G_easting_to_col ( double east,
    struct Cell_head *window)
{
    east = G_adjust_easting (east, window);
    return (east - window->west) / window->ew_res;
}

/* note: row is a double.
 *       row+0.5 will give center
 *       row+0.0 will give northern edge of row
 *       row+1.0 will give southern edge of row
 */
double G_row_to_northing ( double row,
    struct Cell_head *window)
{
    return window->north - row * window->ns_res;
}

double G_col_to_easting (double col,
    struct Cell_head *window)
{
    return window->west + col * window->ew_res;
}

int G_window_rows ()
{
    G__init_window () ;
    return WINDOW_NROWS;
}

int G_window_cols ()
{
    G__init_window () ;
    return WINDOW_NCOLS;
}

int G__init_window ()
{
    if (!G__.window_set)
    {
        G__.window_set = 1;
        G_get_window (&G__.window);
    }

    return 0;
}

/* this routine works fine if the mask is not set
 * may give incorrect results with a mask, since the
 * mask row may have a different repeat value
 * can be fixed by doing it for the mask as well and using
 * the smaller value
 */

int G_row_repeat_nomask (int fd, int row)
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
