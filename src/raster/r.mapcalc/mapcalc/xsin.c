#include "glob.h"

/**********************************************************************
sin(x) 

  if floating point exception occurs during the evaluation of sin(x)
  the result is 0

  note: x is in degrees.
**********************************************************************/

#define DEGREES_TO_RADIANS ( 3.14159 / 180.0 )

extern double sin();
static double cur = 0.0;
static double cur_sin = 0.0; /* sin(0.0); */

x_sin (argc, argv, cell, ncols)
    double *argv[];
    register double *cell;
    register int ncols;
{
    register double *a;

    a = argv[0];
    while (ncols-- > 0)
    {
	if (*a == cur)
	{
	    a++;
	}
	else
	{
	    cur = *a++;
	    floating_point_exception = 0;
	    cur_sin = sin (cur * DEGREES_TO_RADIANS);
	    if (floating_point_exception)
		cur_sin = 0.0;
	}
	*cell++ = cur_sin;
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
