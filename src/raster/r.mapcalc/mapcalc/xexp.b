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
    double exp();
    register double x, *a, *c;
    register int n;

    a = argv[0];
    c = cell;
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
	*c++ = x;
    }
}

n_exp(n,name) char *name;
{
    if (n == 1) return 1;
    fprintf (stderr, "%s - ",name);
    if (n < 1)
	fprintf (stderr, "no arguments specified. ");
    else
	fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s(x)\n", name);
    return 0;
}
