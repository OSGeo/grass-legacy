#include "gis.h"
/**********************************************************************
float(x)

  converts CELL to double
**********************************************************************/

x_2float (argc, argv, cell, ncols)
    double *argv[];
    register double *cell;
    register int ncols;
{
    register double *a;
    a = argv[0];
    while (ncols-- > 0)
	*cell++ = *a++;
}

n_2float(n,name) char *name;
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
