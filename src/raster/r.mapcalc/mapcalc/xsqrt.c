#include "glob.h"

/**********************************************************************
sqrt(x) 

  if floating point exception occurs during the evaluation of tan(x)
  the result is 0

  if x is negative, the result is -sqrt(-x)

**********************************************************************/

x_sqrt (argc, argv, cell, ncols)
    double *argv[];
    register double *cell;
    register int ncols;
{
    double sqrt();
    register double x;
    register double *a;

    a = argv[0];
    while (ncols-- > 0)
    {
	floating_point_exception = 0;
	x = *a++;
	if (x < 0)
	    x = -sqrt (-x);
	else
	    x = sqrt(x);
	if (floating_point_exception)
	    x = 0.0;
	*cell++ = x;
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
