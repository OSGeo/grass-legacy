#include "gis.h"

ask_window (window)
    struct Cell_head *window ;
{
    double south, west ;
    char buff[64] ;
    short ok ;
    struct Cell_head minimal ;



    round (&window->north);
    round (&window->south);
    round (&window->west);
    round (&window->east);
    round (&window->ew_res);
    round (&window->ns_res);
    G_copy (&minimal, window, sizeof (minimal));

    window->rows  = 0 ;
    window->cols  = 0 ;

ok = 0;
while(!ok)
{
/* List window options on the screen for the user to answer */

ok = 1;
V_clear() ;
V_line( 0, "Please set the target region");
V_line( 2, "           ============================= MINIMAL WINDOW ========") ;
V_line( 3, "           |                  North:                           |") ;
V_line( 4, "           |                                                   |") ;
V_line( 5, "           |           ======= GEOREF WINDOW =======           |") ;
V_line( 6, "           |           | NORTH EDGE:               |           |") ;
V_line( 7, "           |           |                           |           |") ;
V_line( 8, "    West   |WEST EDGE  |                           |EAST EDGE  |   East") ;
V_line( 9, "           |           |                           |           |") ;
V_line(10, "           |           | SOUTH EDGE:               |           |") ;
V_line(11, "           |           =============================           |") ;
V_line(12, "           |                                                   |") ;
V_line(13, "           |                  South:                           |") ;
V_line(14, "           =====================================================") ;

V_line(16, "                   Minimal   GRID RESOLUTION   Region           ") ;
V_line(17, "                            --- East-West ---                   ") ;
V_line(18, "                            -- North-South --                   ") ;
V_line (20, "(Minimal region is just large enough to hold entire image)");

    /* V_ques ( variable, type, row, col, length) ; */
    V_ques ( &window->north ,  'd',  6, 36, 11) ;
    V_ques ( &window->south ,  'd', 10, 36, 11) ;
    V_ques ( &window->west  ,  'd',  9, 12, 11) ;
    V_ques ( &window->east  ,  'd',  9, 52, 11) ;
    V_ques ( &window->ew_res,  'd', 17, 47,  7) ;
    V_ques ( &window->ns_res,  'd', 18, 47,  7) ;

    V_const (&minimal.north, 'd',  3, 36, 11) ;
    V_const (&minimal.south, 'd', 13, 36, 11) ;
    V_const (&minimal.west , 'd',  9,  1, 11) ;
    V_const (&minimal.east , 'd',  9, 66, 11) ;
    V_const (&minimal.ew_res,'d', 17, 19,  7) ;
    V_const (&minimal.ns_res,'d', 18, 19,  7) ;
  
    V_intrpt_ok();
    if(!V_call())
	exit(1);

    round (&window->north);
    round (&window->south);
    round (&window->east);
    round (&window->west);
    round (&window->ew_res);
    round (&window->ns_res);

    if ( (window->ns_res <= 0 ) || (window->ew_res <= 0) )
    {
        printf("Illegal resolution value(s)\n") ;
	ok = 0;
    }
    if (window->north <= window->south)
    {
	printf ("North must be larger than south\n");
	ok = 0;
    }
    if (window->east <= window->west)
    {
	printf ("East must be larger than west\n");
	ok = 0;
    }
    if (!ok)
    {
	printf ("hit RETURN -->");
	G_gets(buff);
	continue;
    }

/* if the north-south is not multiple of the resolution,
 *    round the south downward
 */
    south = window->south;
    window->rows = (window->north - window->south + window->ns_res/2)
			/ window->ns_res;
    window->south = window->north - window->rows * window->ns_res;

/* do the same for the west */
    west = window->west ;
    window->cols = (window->east - window->west + window->ew_res/2)
			/ window->ew_res;
    window->west = window->east - window->cols * window->ew_res;

    printf("\n\n") ;

    printf("  north:       %12.2lf\n", window->north);
    printf("  south:       %12.2lf", window->south);

    if (!visually_equal(window->south, south))
        printf("  (Changed to match resolution)");
    printf("\n");

    printf("  east:        %12.2lf\n", window->east);
    printf("  west:        %12.2lf", window->west);

    if (!visually_equal(window->west, west))
        printf("  (Changed to match resolution)");
    printf("\n");

    printf("\n");
    printf("  e-w res:     %12.2lf\n", window->ew_res);
    printf("  n-s res:     %12.2lf\n", window->ns_res);
    printf("  total rows:  %12d\n",    window->rows);
    printf("  total cols:  %12d\n",    window->cols);
    printf("  total cells: %12d\n",    window->rows * window->cols);
    printf("\n");

    ok = 1;
    if (window->north > minimal.north)
    {
	printf ("warning - north falls outside the minimal region\n");
	ok = 0;
    }
    if (window->south < minimal.south)
    {
	printf ("warning - south falls outside the minimal region\n");
	ok = 0;
    }
    if (window->east > minimal.east)
    {
	printf ("warning - east falls outside the minimal region\n");
	ok = 0;
    }
    if (window->west < minimal.west)
    {
	printf ("warning - west falls outside the minimal region\n");
	ok = 0;
    }

    ok = G_yes("\nDo you accept this region? ", ok);
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

static
round (x)
    double *x;
{
    char xs[40];
    sprintf (xs, "%.2lf", *x);
    sscanf (xs, "%lf", x);
}
