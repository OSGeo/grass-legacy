#include "glob.h"
/****************************************************************
abs(x)

   absolute value. if x is negative returns -x
****************************************************************/

int 
i_abs (int argc, CELL *argv[], register CELL *cell, register int ncols)
{
    register CELL *a,x;

    a = argv[0];
    while (ncols-- > 0)
    {
	if (ISNULL(a))
	    SETNULL(cell);
	else
	{
	    if ((x = *a) < 0)
		x = -x;
	    *cell = x;
	}
	cell++;
	a++;
    }

    return 0;
}

int 
x_abs (int argc, double *argv[], register double *xcell, register int ncols)
{
    register double *a,x;

    a = argv[0];
    while (ncols-- > 0)
    {
	if (ISNULL_D(a))
	    SETNULL_D(xcell);
	else
	{
	    if ((x = *a) < 0)
		x = -x;
	    *xcell = x;
	}
	xcell++;
	a++;
    }

    return 0;
}

int 
n_abs (int n, char *name)
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
