#include "gis.h"
#include "glob.h"
/****************************************************************
ewres() east-west resolution
nsres() north-south resolution
tbres() top-bottom resolution

****************************************************************/

x_ewres (argc, argv, cell, ncols)
    double *argv[];
    register double *cell;
    register int ncols;
{
    while (ncols-- > 0)
    {
	*cell++ = current_region.ew_res;
    }
}

n_ewres(n,name) char *name;
{
    if (n == 0) return 1;
    fprintf (stderr, "%s - ",name);
    fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s()\n",name);
    return 0;
}

x_nsres (argc, argv, cell, ncols)
    double *argv[];
    register double *cell;
    register int ncols;
{
    while (ncols-- > 0)
    {
	*cell++ = current_region.ns_res;
    }
}

n_nsres(n,name) char *name;
{
    if (n == 0) return 1;
    fprintf (stderr, "%s - ",name);
    fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s()\n",name);
    return 0;
}

x_tbres (argc, argv, cell, ncols)
    double *argv[];
    register double *cell;
    register int ncols;
{
    while (ncols-- > 0)
    {
	*cell++ = current_region.tb_res;
    }
}

n_tbres(n,name) char *name;
{
    if (n == 0) return 1;
    fprintf (stderr, "%s - ",name);
    fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s()\n",name);
    return 0;
}
