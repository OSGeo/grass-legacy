#include "gis.h"
#include "glob.h"
/****************************************************************
ewres() east-west resolution
nsres() north-south resolution

****************************************************************/

int 
x_ewres (int argc, double *argv[], register double *cell, register int ncols)
{
    while (ncols-- > 0)
    {
	*cell++ = current_region.ew_res;
    }

    return 0;
}

int 
n_ewres (int n, char *name)
{
    if (n == 0) return 1;
    fprintf (stderr, "%s - ",name);
    fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s()\n",name);
    return 0;
}

int 
x_nsres (int argc, double *argv[], register double *cell, register int ncols)
{
    while (ncols-- > 0)
    {
	*cell++ = current_region.ns_res;
    }

    return 0;
}

int 
n_nsres (int n, char *name)
{
    if (n == 0) return 1;
    fprintf (stderr, "%s - ",name);
    fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s()\n",name);
    return 0;
}
