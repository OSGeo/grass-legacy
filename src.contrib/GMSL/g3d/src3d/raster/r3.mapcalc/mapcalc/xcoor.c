#include "gis.h"
#include "glob.h"
/****************************************************************
x() easting at center of column
y() northing at center of row
z() topping at center of depth;

****************************************************************/

extern double G_col_to_easting();
extern double G_row_to_northing();

x_x (argc, argv, cell, ncols)
    double *argv[];
    register double *cell;
    register int ncols;
{
    double x;

    x=current_region.west+0.5*current_region.ew_res;
    
    while (ncols-- > 0)
    {
	*cell++ = x;
	x += current_region.ew_res;
    }
}

n_x(n,name) char *name;
{
    if (n == 0) return 1;
    fprintf (stderr, "%s - ",name);
    fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s()\n",name);
    return 0;
}

x_y (argc, argv, cell, ncols)
    double *argv[];
    register double *cell;
    register int ncols;
{
    double y;

    y=current_region.north-(current_row+0.5)*current_region.ns_res;
    while (ncols-- > 0)
    {
	*cell++ = y;
    }
}

n_y(n,name) char *name;
{
    if (n == 0) return 1;
    fprintf (stderr, "%s - ",name);
    fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s()\n",name);
    return 0;
}

x_z (argc, argv, cell, ncols)
    double *argv[];
    register double *cell;
    register int ncols;
{
    double z;

    z=current_region.bottom+0.5*current_region.tb_res;
    
    while (ncols-- > 0)
    {
	*cell++ = z;
	z += current_region.tb_res;
    }
}

n_z(n,name) char *name;
{
    if (n == 0) return 1;
    fprintf (stderr, "%s - ",name);
    fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s()\n",name);
    return 0;
}

