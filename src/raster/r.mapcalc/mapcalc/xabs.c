#include "gis.h"
/****************************************************************
abs(x)

   absolute value. if x is negative returns -x
****************************************************************/

i_abs (argc, argv, cell, ncols)
    CELL *argv[];
    register CELL *cell;
    register int ncols;
{
    register CELL *a,x;

    a = argv[0];
    while (ncols-- > 0)
    {
	if ((x = *a) < 0)
	    x = -x;
	*cell++ = x;
	a++;
    }
}

x_abs (argc, argv, cell, ncols)
    double *argv[];
    register double *cell;
    register int ncols;
{
    register double *a,x;

    a = argv[0];
    while (ncols-- > 0)
    {
	if ((x = *a) < 0)
	    x = -x;
	*cell++ = x;
	a++;
    }
}

n_abs(n,name) char *name;
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
