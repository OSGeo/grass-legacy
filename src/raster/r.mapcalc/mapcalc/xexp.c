#include "glob.h"
#include <math.h>

/**********************************************************************
exp(x) 

  computes e raised to power x

  if floating point exception, the result is 0
**********************************************************************/

static int _exp (
    register double *a,
    register double *xcell,int ncols)
{
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

    return 0;
}

static int _pow (
    register double *a,register double *b,
    register double *xcell,int ncols)
{
    for ( ; ncols-- > 0; a++, b++, xcell++)
    {
	if (ISNULL_D(a) || ISNULL_D(b))
	    SETNULL_D(xcell);
	else
	{
	    if (*a < 0.0 && *b != ceil(*b))
	    {
		SETNULL_D(a);
	    }
	    else
	    {
		floating_point_exception = 0;
		*xcell = pow(*a,*b);
		if (floating_point_exception)
		    SETNULL_D(xcell);
	    }
	}
    }

    return 0;
}

int n_exp(int n,char *name)
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

int x_exp (int argc, double *argv[],double *xcell,int ncols)
{
    if (argc == 1)
	_exp(argv[0], xcell, ncols);
    else
	_pow(argv[0], argv[1], xcell, ncols);

    return 0;
}
