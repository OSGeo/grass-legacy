#include "glob.h"

/**********************************************************************
sin(x) 

  if floating point exception occurs during the evaluation of sin(x)
  the result is NULLVALUE

  note: x is in degrees.
**********************************************************************/

#define DEGREES_TO_RADIANS ( 3.14159 / 180.0 )

extern double sin();

x_sin (argc, argv, xcell, ncols)
    double *argv[];
    register double *xcell;
    register int ncols;
{
    register double *a;

    a = argv[0];
    for ( ; ncols-- > 0; a++, xcell++)
    {
	if (ISNULL_D (a))
	    SETNULL_D(xcell);
	else
	{
	    floating_point_exception = 0;
	    *xcell = sin ((*a) * DEGREES_TO_RADIANS);
	    if (floating_point_exception)
		SETNULL_D(xcell);
	}
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
