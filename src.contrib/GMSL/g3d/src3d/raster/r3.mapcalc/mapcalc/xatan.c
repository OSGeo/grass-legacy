
#include "glob.h"

/**********************************************************************
atan(x)     range [-90,90]
atan(y,x) = atan(y/x) range[0,360]

  if floating point exception occurs during the evaluation of atan(x)
  the result is 0

  note: result is in degrees
**********************************************************************/

#define RADIANS_TO_DEGREES ( 180.0 / 3.14159 )

x_atan (argc, argv, xcell, ncols)
    double *argv[];
    register double *xcell;
    register int ncols;
{
    double atan(), atan2();
    register double *a;
    register double *b;

    a = argv[0];
    if (argc == 1)
    {
	for ( ; ncols-- > 0; a++, xcell++)
	{
	    if (ISNULL_D(a))
		SETNULL_D(xcell);
	    else
	    {
		floating_point_exception = 0;
		*xcell = RADIANS_TO_DEGREES * atan (*a);
		if (floating_point_exception)
		    SETNULL_D(xcell);
	    }
	}
    }
    else
    {
	b = argv[1];
	for ( ; ncols-- > 0; a++, b++, xcell++)
	{
	    if (ISNULL_D(a) || ISNULL_D(b))
	    {
		SETNULL_D(xcell);
	    }
	    else
	    {
		floating_point_exception = 0;
		if (*a != 0.0 || *b != 0.0)
		    *xcell = RADIANS_TO_DEGREES * atan2 (*b, *a);
                else 
		    SETNULL_D(xcell);
		if (floating_point_exception)
		    SETNULL_D(xcell);
		else if (*xcell < 0)
		    *xcell += 360.0;
	    }
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
