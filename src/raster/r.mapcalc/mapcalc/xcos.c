#include "glob.h"

/**********************************************************************
cos(x) 

  if floating point exception occurs during the evaluation of cos(x)
  the result is 0

  note: x is in degrees.
**********************************************************************/

#define DEGREES_TO_RADIANS ( 3.14159 / 180.0 )

extern double cos();

int 
x_cos (int argc, double *argv[], register double *xcell, register int ncols)
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
	    *xcell = cos ((*a) * DEGREES_TO_RADIANS);
	    if (floating_point_exception)
		SETNULL_D(xcell);
	}
    }

    return 0;
}

int 
n_cos (int n, char *name)
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
