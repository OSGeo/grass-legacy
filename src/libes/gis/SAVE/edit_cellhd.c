/* %W% %G%
 **********************************************************************
 *
 *   G_edit_cellhd (cellhd, as_cellhd)
 *      struct Cell_head *cellhd   (cellhd to be defined)
 *      int as_cellhd              (1 = cellhd, 0 = window)
 *
 *   Screen oriented user interactive session for modifying a cell header
 *   or window.
 *   Uses the visual_ask V_ask routines.  As such, programs including
 *   this must load the GRASS library $(VASK)
 *
 *   returns:
 *      -1 error of some sort
 *       0 ok
 **********************************************************************/
#include "gis.h"
#include "windround.h"

G_edit_cellhd (cellhd, as_cellhd)
    struct Cell_head *cellhd ;
{
    struct Cell_head default_window ;
    double south, west ;
    char buff[64] ;
    short ok ;
    char *prj, *G_projection_name();

    if (G_get_default_window(&default_window) != 1)
        return -1 ;

    cellhd->rows        = 0 ;
    cellhd->cols        = 0 ;
    if(cellhd->proj < 0)
    {
	cellhd->proj        = default_window.proj ;
	cellhd->zone        = default_window.zone ;
    }
    else if(cellhd->zone < 0)
	cellhd->zone        = default_window.zone ;

    if (cellhd->west >= cellhd->east || cellhd->south >= cellhd->north) 
    {
        cellhd->north       = default_window.north     ; 
        cellhd->south       = default_window.south     ;
        cellhd->west        = default_window.west      ; 
        cellhd->east        = default_window.east      ;
        cellhd->ew_res      = default_window.ew_res    ;
        cellhd->ns_res      = default_window.ns_res    ;
    }
    
ok = 0;
while(!ok)
{
/* List window options on the screen for the user to answer */

ok = 1;
V_clear() ;
if (as_cellhd)
V_line( 0, "                           IDENTIFY CELL HEADER") ;
else
V_line( 0, "                              IDENTIFY WINDOW") ;

V_line( 2, "           ============================= DEFAULT WINDOW ========") ;
V_line( 3, "           |          Default North:                           |") ;
V_line( 4, "           |                                                   |") ;

if (as_cellhd)
V_line( 5, "           |           =======  CELL HEADER  =======           |") ;
else
V_line( 5, "           |           =======  YOUR WINDOW  =======           |") ;

V_line( 6, "           |           | NORTH EDGE:               |           |") ;
V_line( 7, "           |           |                           |           |") ;
V_line( 8, " Def. West |WEST EDGE  |                           |EAST EDGE  | Def. East") ;
V_line( 9, "           |           |                           |           |") ;
V_line(10, "           |           | SOUTH EDGE:               |           |") ;
V_line(11, "           |           =============================           |") ;
V_line(12, "           |                                                   |") ;
V_line(13, "           |          Default South:                           |") ;
V_line(14, "           =====================================================") ;

if (as_cellhd)
V_line(16, "                   Default   GRID RESOLUTION   Cell Header      ") ;
else
V_line(16, "                   Default   GRID RESOLUTION   Window           ") ;

V_line(17, "                            --- East-West ---                   ") ;
V_line(18, "                            -- North-South --                   ") ;

if (as_cellhd)
{
V_line(20, "                            -- PROJECTION  --                   ") ;
V_line(21, "                            --    ZONE     --                   ") ;
}

    /* V_ques ( variable, type, row, col, length) ; */
    V_ques ( &cellhd->north ,  'd',  6, 36, 11) ;
    V_ques ( &cellhd->south ,  'd', 10, 36, 11) ;
    V_ques ( &cellhd->west  ,  'd',  9, 12, 11) ;
    V_ques ( &cellhd->east  ,  'd',  9, 52, 11) ;
    V_ques ( &cellhd->ew_res,  'd', 17, 47,  7) ;
    V_ques ( &cellhd->ns_res,  'd', 18, 47,  7) ;
    if (as_cellhd)
    {
	V_ques (&cellhd->proj,  'i', 20, 47,  3) ;
	V_ques (&cellhd->zone,  'i', 21, 47,  3) ;
    }

    V_const (&default_window.north, 'd',  3, 36, 11) ;
    V_const (&default_window.south, 'd', 13, 36, 11) ;
    V_const (&default_window.west , 'd',  9,  1, 11) ;
    V_const (&default_window.east , 'd',  9, 66, 11) ;
    V_const (&default_window.ew_res,'d', 17, 19,  7) ;
    V_const (&default_window.ns_res,'d', 18, 19,  7) ;
    if (as_cellhd)
    {
	V_const (&default_window.proj,  'i', 20, 21,  3) ;
	V_const (&default_window.zone,  'i', 21, 21,  3) ;
    }

    V_call() ;
    V_clear();

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
	printf ("hit RETURN -->");
	if (!gets(buff)) exit(0);
	G_strip (buff);
	if (strcmp (buff, "exit") == 0) exit(0);
	continue;
    }

/* if the north-south is not multiple of the resolution,
 *    round the south downward
 */
    south = cellhd->south;
    cellhd->rows = (cellhd->north - cellhd->south) / cellhd->ns_res
		   + WINDOW_ROUND ;
    cellhd->south = cellhd->north - cellhd->rows * cellhd->ns_res;

/* do the same for the west */
    west = cellhd->west ;
    cellhd->cols = (cellhd->east - cellhd->west) / cellhd->ew_res
		   + WINDOW_ROUND ;
    cellhd->west = cellhd->east - cellhd->cols * cellhd->ew_res;

SHOW:
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
    if (cellhd->north > default_window.north)
    {
	printf ("warning - north falls outside the default window\n");
	ok = 0;
    }
    if (cellhd->south < default_window.south)
    {
	printf ("warning - south falls outside the default window\n");
	ok = 0;
    }
    if (cellhd->east > default_window.east)
    {
	printf ("warning - east falls outside the default window\n");
	ok = 0;
    }
    if (cellhd->west < default_window.west)
    {
	printf ("warning - west falls outside the default window\n");
	ok = 0;
    }
    if (as_cellhd)
    {
	if (cellhd->proj != default_window.proj)
	{
	    printf ("warning - projection differs from the default projection\n");
	    ok = 0;
	}
	else if (cellhd->zone != default_window.zone)
	{
	    printf ("warning - zone differs from the default zone\n");
	    ok = 0;
	}
    }
ASK:
    printf("\nDo you accept this %s? (y/n) [%s] > ",
	as_cellhd?"cell header":"window", ok?"y":"n") ;
    if(!G_gets(buff))
	goto SHOW;
    G_strip (buff);
    switch (*buff)
    {
	case 0:
		break;
	case 'y':
	case 'Y':
	    ok = 1 ;
		break;
	case 'n':
	case 'N':
		ok = 0;
		break;
	default:
		goto ASK;
	}
}
return 0;
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
