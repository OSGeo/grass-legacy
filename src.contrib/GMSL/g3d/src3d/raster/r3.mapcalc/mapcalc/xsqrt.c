#include "glob.h"

/**********************************************************************
sqrt(x) 

  if floating point exception occurs during the evaluation of sqrt(x)
  the result is NULLVALUE

  if x is negative, the result is -sqrt(-x)

**********************************************************************/

x_sqrt (argc, argv, xcell, ncols)
    double *argv[];
    register double *xcell;
    register int ncols;
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
		x = -sqrt (-x);
	    else
		x = sqrt(x);
	    if (floating_point_exception)
		SETNULL_D(xcell);
	    else
		*xcell = x;
	}
    }
}

n_sqrt(n,name) char *name;
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
