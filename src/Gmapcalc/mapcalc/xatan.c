#include "glob.h"

/**********************************************************************
atan(x)     range [-90,90]
atan(y,x) = atan(y/x) range[0,360]

  if floating point exception occurs during the evaluation of atan(x)
  the result is 0

  note: result is in degrees
**********************************************************************/

#define RADIANS_TO_DEGREES ( 180.0 / 3.14159 )

x_atan (argc, argv, cell, ncols)
    double *argv[];
    register double *cell;
    register int ncols;
{
    double atan(), atan2();
    register double x;
    register double *a;
    register double *b;

    a = argv[0];
    if (argc == 1)
    {
	while (ncols-- > 0)
	{
	    floating_point_exception = 0;
	    x = RADIANS_TO_DEGREES * atan (*a++);
	    if (floating_point_exception)
		x = 0.0;
	    *cell++ = x;
	}
    }
    else
    {
	b = argv[1];
	while (ncols-- > 0)
	{
	    floating_point_exception = 0;
	    if (*a != 0.0 || *b != 0.0)
		x = RADIANS_TO_DEGREES * atan2 (*b, *a);
	    else
		x = 0;
	    a++;b++;
	    if (floating_point_exception)
		x = 0.0;
	    if (x < 0) x += 360;
	    *cell++ = x;
	}
    }
}

n_atan(n,name) char *name;
{
    if (n == 1 || n == 2) return 1;
    fprintf (stderr, "%s - ",name);
    if (n < 1)
	fprintf (stderr, "no arguments specified. ");
    else
	fprintf (stderr, "too many arguments specified. ");
    fprintf (stderr, "usage: %s(x) or %s(y,x)\n", name, name);
    return 0;
}
