#include "gis.h"
#include "windround.h"

check_window (title, window)
    char *title;
    struct Cell_head *window ;
{
    int i;
    double south, west ;
    int ok ;

    ok = 1;
    if ( (window->ns_res <= 0 ) || (window->ew_res <= 0) )
	ok = 0;
    if (window->north <= window->south)
	ok = 0;
    if (window->east <= window->west)
	ok = 0;

/* if the north-south is not multiple of the resolution,
 * round the south downward.
 * do the same for the west
 */
    if (ok)
    {
	south = window->south;
	window->rows = (window->north - window->south) / window->ns_res
		       + WINDOW_ROUND ;
	window->south = window->north - window->rows * window->ns_res;

	west = window->west ;
	window->cols = (window->east - window->west) / window->ew_res
		       + WINDOW_ROUND ;
	window->west = window->east - window->cols * window->ew_res;
    }

    printf ("title: %s\n", title);
    printf ("\n") ;

    printf ("north:       %12.2lf", window->north);
    if (window->north <= window->south)
	printf (" <-- north must be greater than the south");
    printf ("\n");

    printf ("south:       %12.2lf", window->south);
    if (window->north <= window->south)
	printf (" <--");
    else if (!visually_equal(window->south, south))
        printf ("  (Changed to match resolution)");
    printf ("\n");

    printf ("east:        %12.2lf", window->east);
    if (window->east <= window->west)
	printf (" <-- east must be greater than the west");
    printf ("\n");

    printf ("west:        %12.2lf", window->west);
    if (window->east <= window->west)
	printf (" <--");
    else if (!visually_equal(window->west, west))
        printf ("  (Changed to match resolution)");
    printf ("\n");

    printf ("e-w res:     %12.2lf", window->ew_res);
    if (window->ew_res <= 0.0)
	printf (" <-- illegal value - must be postive");
    printf ("\n");
    printf ("n-s res:     %12.2lf", window->ns_res);
    if (window->ns_res <= 0.0)
	printf (" <-- illegal value - must be postive");
    printf ("\n");
    if (ok)
    {
	printf ("\n");
	printf ("(%d rows, %d cols, %d total cells)\n", 
	    window->rows, window->cols, window->rows * window->cols);
	printf ("\nDo these values look ok to you? ");
	if (yes_no()) return 1;
    }

    printf ("\nDo you wish to change some of these values? ");
    return yes_no()?0:-1;
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
