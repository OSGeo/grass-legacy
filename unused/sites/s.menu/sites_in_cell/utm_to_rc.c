#include "gis.h"
#include "site_count.h"

struct RCLIST *
utm_to_rc (struct Cell_head *window, double north, double east, struct RCLIST *rc, int *count)
{
    int n;
    int row;
    int col;
    float northing_to_row ();
    float easting_to_col ();

    row= (int) northing_to_row(north,window);
    col= (int) easting_to_col(east,window);

    n = *count;
    while (n-- > 0)
    {
	if((row==rc[n].row) && (col==rc[n].col))
	{
             rc[n].val++;
	     return rc;
	}
    }

    n = (*count)++;
    rc = (struct RCLIST *) G_realloc (rc, (*count) * sizeof (struct RCLIST));
    rc[n].row = row;
    rc[n].col = col;
    rc[n].val = 1;
    return rc;
}
