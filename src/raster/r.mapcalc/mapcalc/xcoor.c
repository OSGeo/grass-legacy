#include "gis.h"
#include "glob.h"
/****************************************************************
x() easting at center of column
y() northing at center of row

****************************************************************/


int 
x_x (int argc, double *argv[], register double *cell, register int ncols)
{
    double x;

    x = G_col_to_easting (0.5, &current_region);
    while (ncols-- > 0)
    {
	*cell++ = x;
	x += current_region.ew_res;
    }

    return 0;
}

int 
n_x (int n, char *name)
{
    if (n == 0) return 1;
    fprintf (stderr, "%s - ",name);
    fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s()\n",name);
    return 0;
}

int 
x_y (int argc, double *argv[], register double *cell, register int ncols)
{
    double y;

    y = G_row_to_northing (current_row + 0.5, &current_region);
    while (ncols-- > 0)
    {
	*cell++ = y;
    }

    return 0;
}

int 
n_y (int n, char *name)
{
    if (n == 0) return 1;
    fprintf (stderr, "%s - ",name);
    fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s()\n",name);
    return 0;
}
