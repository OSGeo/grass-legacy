#include "glob.h"

/**********************************************************************
exp(x) 

  computes e raised to power x

  if floating point exception, the result is 0
**********************************************************************/

x_exp (argc, argv, xcell, ncols)
    double *argv[];
    double *xcell;
{
    if (argc == 1)
	_exp(argv[0], xcell, ncols);
    else
	_pow(argv[0], argv[1], xcell, ncols);
}

static
_exp (a, xcell, ncols)
    register double *a;
    register double *xcell;
{
    double exp();

    for ( ; ncols-- > 0; a++, xcell++)
    {
	if (ISNULL_D(a))
	    SETNULL_D(xcell);
	else
	{
	    floating_point_exception = 0;
	    *xcell = exp(*a);
	    if (floating_point_exception)
		SETNULL_D(xcell);
	}
    }
}

static
_pow (a, b, xcell, ncols)
    register double *a, *b;
    register double *xcell;
{
    double pow();

    for ( ; ncols-- > 0; a++, b++, xcell++)
    {
	if (ISNULL_D(a) || ISNULL_D(b))
	    SETNULL_D(xcell);
	else
	{
	    floating_point_exception = 0;
	    *xcell = pow(*a,*b);
	    if (floating_point_exception)
		SETNULL_D(xcell);
	}
    }
}

n_exp(n,name) char *name;
{
    if (n == 1 || n == 2) return 1;
    fprintf (stderr, "%s - ",name);
    if (n < 1)
	fprintf (stderr, "no arguments specified. ");
    else if (n > 2)
	fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s(x) or %s(x,y)\n", name, name);
    return 0;
}
