/* %W% %G%
 **********************************************************************
 *
 *   edit_cellhd (cellhd)
 *      struct Cell_head *cellhd   (cellhd to be defined)
 *
 *   Screen oriented user interactive session for modifying a cell header
 *   Uses the visual_ask V_ask routines.  As such, programs including
 *   this must load the GRASS library lib_vask.a followed by -lcurses
 *   and -ltermlib.
 *
 *   note: if the rows and cols of the cellhd are positive, then
 *         the rows and cols implied by the values entered by the user
 *         for the north,south,east,west and resolutions must match.
 *         otherwise the rows and cols are simply computed from the
 *         values entered by the user
 *
 *   returns:
 *      -1 error of some sort
 *       0 ok
 **********************************************************************/
#include "gis.h"
#include "windround.h"

edit_cellhd (cellhd)
    struct Cell_head *cellhd ;
{
    int n;
    double south, west ;
    short ok ;
    char *prj, *G_projection_name();
    int rows, cols;
    struct Cell_head window ;
    char coltext[30];
    char rowtext[30];

    if (G_get_default_window (&window) < 0)
	G_fatal_error ("Can't read default window for database");

    rows = cellhd->rows;
    cols = cellhd->cols;
    if (rows <= 0)
	rows = 0;
    if (cols <= 0)
	cols = 0;

/* List window options on the screen for the user to answer */

    V_clear() ;
    V_line( 0,"                           IDENTIFY CELL HEADER") ;

    V_line( 5,"                   ================================") ;
    V_line( 6,"                   | NORTH EDGE:                  |") ;
    V_line( 7,"                   |                              |") ;
    V_line( 8,"        WEST EDGE  |                              | EAST EDGE") ;
    V_line( 9,"                   |                              |") ;
    V_line(10,"                   | SOUTH EDGE:                  |") ;
    V_line(11,"                   ================================") ;

    V_line(13," GRID RESOLUTION east-west:               COLS:");
    V_line(14,"                 north-south:             ROWS:");
    V_line(16," PROJECTION:") ;
    V_line(17," ZONE:") ;

    /* V_ques ( variable, type, row, col, length) ; */
    V_ques ( &cellhd->north ,  'd',  6, 33, 11) ;
    V_ques ( &cellhd->south ,  'd', 10, 33, 11) ;
    V_ques ( &cellhd->west  ,  'd',  9,  8, 11) ;
    V_ques ( &cellhd->east  ,  'd',  9, 52, 11) ;
    V_ques ( &cellhd->ew_res,  'd', 13, 30,  7) ;
    V_ques ( &cellhd->ns_res,  'd', 14, 30,  7) ;
    V_const (coltext, 's', 13, 48, 30);
    V_const (rowtext, 's', 14, 48, 30);
    V_ques (&cellhd->proj,  'i', 16, 14,  3) ;
    V_ques (&cellhd->zone,  'i', 17, 14,  3) ;

    for (;;)
    {
	if (cols && (cellhd->cols != cols))
	    sprintf (coltext, "%-4d (should be %d)", cellhd->cols, cols);
	else
	    sprintf (coltext, "%-4d", cellhd->cols);

	if (rows && (cellhd->rows != rows))
	    sprintf (rowtext, "%-4d (should be %d)", cellhd->rows, rows);
	else
	    sprintf (rowtext, "%-4d", cellhd->rows);

	ok = 1;
	V_intrpt_ok();
	if(!V_call())
	    return -1;

	if ( (cellhd->ns_res <= 0 ) || (cellhd->ew_res <= 0) )
	{
	    printf("Illegal resolution value(s)\n") ;
	    ok = 0;
	}
	if (cellhd->north <= cellhd->south)
	{
	    printf ("North must be larger than south\n");
	    ok = 0;
	}
	if (cellhd->east <= cellhd->west)
	{
	    printf ("East must be larger than west\n");
	    ok = 0;
	}
	if (!ok)
	{
	    hitreturn();
	    continue;
	}
/* compute the number of rows and columns */
	cellhd->rows = (cellhd->north - cellhd->south) / cellhd->ns_res
		   + WINDOW_ROUND ;

	cellhd->cols = (cellhd->east - cellhd->west) / cellhd->ew_res
		   + WINDOW_ROUND ;

/* if the rows and cols must not change, enforce this now */
	if (rows && (rows != cellhd->rows))
	{
	    printf ("\n");
	    printf ("North-south values indicate %d rows.\n", cellhd->rows);
	    printf ("Please adjust them so they give %d rows\n", rows);
	    ok = 0;
	}

	if (cols && (cellhd->cols != cols))
	{
	    printf ("\n");
	    printf ("East-west values indicate %d columns.\n", cellhd->cols);
	    printf ("Please adjust them so they give %d columns\n", cols);
	    ok = 0;
	}
	if (!ok)
	{
	    hitreturn();
	    continue;
	}

/* if the north-south is not multiple of the resolution,
 *    round the south downward
 * do the same for the west
 */

	west = cellhd->west ;
	cellhd->west = cellhd->east - cellhd->cols * cellhd->ew_res;

	south = cellhd->south;
	cellhd->south = cellhd->north - cellhd->rows * cellhd->ns_res;

/* show the results */
	printf("\n\n") ;
	prj = G_projection_name (cellhd->proj);
	if (!prj) prj = "** unknown **";
	printf("  projection:   %d (%s)\n", cellhd->proj, prj) ;

	printf("  zone:         %d\n", cellhd->zone);

	printf("  north:       %12.2lf\n", cellhd->north);

	printf("  south:       %12.2lf", cellhd->south);
	if (!visually_equal(cellhd->south, south))
	    printf("  (Changed to match resolution)");
	printf("\n");

	printf("  east:        %12.2lf\n", cellhd->east);

	printf("  west:        %12.2lf", cellhd->west);
	if (!visually_equal(cellhd->west, west))
	    printf("  (Changed to match resolution)");
	printf("\n");
	printf("\n");
	printf("  e-w res:     %12.2lf\n", cellhd->ew_res);
	printf("  n-s res:     %12.2lf\n", cellhd->ns_res);
	printf("  total rows:  %12d\n",    cellhd->rows);
	printf("  total cols:  %12d\n",    cellhd->cols);
	printf("  total cells: %12ld\n",    (long) cellhd->rows * cellhd->cols);
	printf("\n");

	ok = 1;
	if (cellhd->north > window.north)
	{
	    printf ("warning - north is above the default window north (%.2lf)\n", window.north);
	    ok = 0;
	}
	if (cellhd->south < window.south)
	{
	    printf ("warning - south is below the default window south (%.2lf)\n", window.south);
	    ok = 0;
	}
	if (cellhd->east > window.east)
	{
	    printf ("warning - east is east of the default window east (%.2lf)\n", window.east);
	    ok = 0;
	}
	if (cellhd->west < window.west)
	{
	    printf ("warning - west is west of the default window west (%.2lf)\n", window.west);
	    ok = 0;
	}
	if (cellhd->proj != window.proj)
	{
	    printf ("warning - projection differs from the default projection (%d)\n", window.proj);
	    ok = 0;
	}
	if (cellhd->zone != window.zone)
	{
	    printf ("warning - zone differs from the default zone (%d)\n", window.zone);
	    ok = 0;
	}
	printf ("\n");
	if (G_yes ("Do you accept these values? ", ok))
	    return 0;
    }
}

static
visually_equal (x, y)
    double x, y;
{
    char xs[40], ys[40];

    if (x == y) return 1;

    sprintf (xs, "%.2lf", x);
    sprintf (ys, "%.2lf", y);

    return strcmp (xs, ys) == 0;
}
