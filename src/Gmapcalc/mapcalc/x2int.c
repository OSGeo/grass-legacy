#include "gis.h"
/**********************************************************************
int(x)

   truncates to nearest integer and returns CELL
**********************************************************************/
i_2int (argc, argv, cell, ncols)
    CELL *argv[];
    register CELL *cell;
    register int ncols;
{
    register CELL *a;
    a = argv[0];
    while (ncols-- > 0)
	*cell++ = *a++;
}

x_2int (argc, argv, cell, ncols)
    double *argv[];
    register CELL *cell;
    register int ncols;
{
    register double *a;
    a = argv[0];
    while (ncols-- > 0)
	*cell++ = (CELL) *a++;
}

n_2int(n,name) char *name;
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
