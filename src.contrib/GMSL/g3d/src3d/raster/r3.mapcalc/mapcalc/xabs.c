#include "glob.h"
/****************************************************************
abs(x)

   absolute value. if x is negative returns -x
****************************************************************/

x_abs (argc, argv, xcell, ncols)
    double *argv[];
    register double *xcell;
    register int ncols;
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
