#include "gis.h"
#include "glob.h"

/**********************************************************************
isnull(x)

  return 1 if x is null, 0 otherwise

**********************************************************************/

x_isnull (argc, argv, cell, ncols)
    double *argv[];
    register double *cell;
    register int ncols;
{
    register double *a;
    a = argv[0];
    while (ncols-- > 0)
	*cell++ = (double) (ISNULL_D(a++) ? 1 : 0);
}

n_isnull(n,name) char *name;
{
    if (n == 1) return 1;
    fprintf (stderr, "%s - ",name);
    if (n < 1)
	fprintf (stderr, "no arguments specified. ");
    else
	fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s(x)\n",name);
    return 0;
}
