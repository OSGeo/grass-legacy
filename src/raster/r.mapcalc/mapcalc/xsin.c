#include "glob.h"

/**********************************************************************
sin(x) 

  if floating point exception occurs during the evaluation of sin(x)
  the result is 0

  note: x is in degrees.
**********************************************************************/

#define DEGREES_TO_RADIANS ( 3.14159 / 180.0 )

x_sin (argc, argv, cell, ncols)
    double *argv[];
    register double *cell;
    register int ncols;
{
    double sin();
    register double x;
    register double *a;

    a = argv[0];
    while (ncols-- > 0)
    {
	floating_point_exception = 0;
	x = sin (*a++ * DEGREES_TO_RADIANS);
	if (floating_point_exception)
	    x = 0.0;
	*cell++ = x;
    }
}

n_sin(n,name) char *name;
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
