#include "glob.h"

/**********************************************************************
log(x) 
log(x,b)

  first form computes the natural log of x = ln(x)
  second form computes log of x base b = ln(x)/ln(b)

  if x is non-positive, or floating point exception occurs while
  computing ln(x), the result is zero

  if b is non-positive, or floating point exception occurs while
  computing ln(b), the result is zero
**********************************************************************/

x_log (argc, argv, cell, ncols)
    double *argv[];
    double *cell;
{
    double log(),x,y;
    register double *a,*b;
    register double *c;
    register int n;

    a = argv[0];
    c = cell;
    n = ncols;
    while (n-- > 0)
    {
	floating_point_exception = 0;
	x = *a++;
	if (x <= 0.0)
	{
	    x = 0.0;
	    overflow_occurred = 1;
	}
	else
	{
	    x = log (x);
	    if (floating_point_exception)
		x = 0.0;
	}
	*c++ = x;
    }
    if (argc == 1) return;
    b=argv[1];
    c=cell;
    n = ncols;
    while (n-- > 0)
    {
	x = *c;
	floating_point_exception = 0;
	y = *b++;
	if (y <= 0.0)
	    x = 0.0;
	else
	{
	    y = log (y);
	    if (!floating_point_exception)
		x = x/y;
	    if (floating_point_exception)
		x = 0.0;
	}
	*c++ = x;
    }
}

n_log(n,name) char *name;
{
    if (n == 1 || n == 2) return 1;
    fprintf (stderr, "%s - ",name);
    if (n < 1)
	fprintf (stderr, "no arguments specified. ");
    else
	fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s(x) or %s(x,base)\n", name, name);
    return 0;
}
