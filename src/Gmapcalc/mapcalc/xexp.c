/* %W% %G% */
#include "glob.h"

/**********************************************************************
exp(x) 

  computes e raised to power x

  if floating point exception occurs
    if x is negative, the result is 0
    else x is a huge positive number
**********************************************************************/

x_exp (argc, argv, cell, ncols)
    double *argv[];
    double *cell;
{
    if (argc == 1)
	_exp(argv[0], cell, ncols);
    else
	_pow(argv[0], argv[1], cell, ncols);
}

static
_exp (a, cell, ncols)
    register double *a;
    register double *cell;
{
    double exp();
    register double x;
    register int n;

    n = ncols;
    while (n-- > 0)
    {
	floating_point_exception = 0;
	x = exp(*a);
	if (floating_point_exception)
	{
	    if (*a < 0)
		x = 0;
	    else
		x = HUGE;
	}
	if (x > HUGE)
	    x = HUGE;
	a++;
	*cell++ = x;
    }
}

static
_pow (a, b, cell, ncols)
    register double *a, *b;
    register double *cell;
{
    double pow();
    register double x;
    register int n;

    n = ncols;
    while (n-- > 0)
    {
	floating_point_exception = 0;
	x = pow(*a++,*b++);
	if (floating_point_exception)
	    x = 0.0;
	*cell++ = x;
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
