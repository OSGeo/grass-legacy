#include "glob.h"

/**********************************************************************
log(x) 
log(x,b)

  first form computes the natural log of x = ln(x)
  second form computes log of x base b = ln(x)/ln(b)

  if x is non-positive, or floating point exception occurs while
  computing ln(x), the result is NULLVALUE

  if b is non-positive, or 1.0, or floating point exception occurs while
  computing ln(b), the result is NULLVALUE
**********************************************************************/

x_log (argc, argv, xcell, ncols)
    double *argv[];
    double *xcell;
{
    double log();
    register double *a,*b;
    register double *c;
    register int n;

    a = argv[0];
    c = xcell;
    n = ncols;
    for ( ; n-- > 0; a++, c++)
    {
	if (ISNULL_D(a) || *a <= 0.0)
	    SETNULL_D(c);
	else
	{
	    floating_point_exception = 0;
	    *c = log (*a);
	    if (floating_point_exception)
		SETNULL_D(c);
	}
    }
    if (argc == 1) return;
    b=argv[1];
    c=xcell;
    n = ncols;
    for ( ; n-- > 0; b++, c++)
    {
	if (ISNULL_D(b) || ISNULL_D(c) || *b <= 0.0 || *b == 1.0)
	    SETNULL_D(c);
	else
	{
	    *c /= log (*b);
	    if (floating_point_exception)
		SETNULL_D(c);
	}
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
