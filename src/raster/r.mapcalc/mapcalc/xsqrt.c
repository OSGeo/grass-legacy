#include "glob.h"

/**********************************************************************
sqrt(x) 

  if x is negative, or floating point exception occurs during the
  evaluation of sqrt(x) the result is NULLVALUE

**********************************************************************/

int 
x_sqrt (int argc, double *argv[], register double *xcell, register int ncols)
{
    double sqrt();
    register double x;
    register double *a;

    a = argv[0];
    for ( ; ncols-- > 0; a++, xcell++)
    {
	if (ISNULL_D(a))
	    SETNULL_D(xcell);
	else
	{
	    floating_point_exception = 0;
	    x = *a;
	    if (x < 0)
		SETNULL_D(xcell);
	    else
	    {
		x = sqrt(x);
		if (floating_point_exception)
		    SETNULL_D(xcell);
		else
		    *xcell = x;
	    }
	}
    }

    return 0;
}

int 
n_sqrt (int n, char *name)
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
