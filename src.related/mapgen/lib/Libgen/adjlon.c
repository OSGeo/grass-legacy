#ifndef lint
static char *SCCSID = "@(#)adjlon.c	AMG v.3.1";
#endif
/*	Reduce argument to range +/- TWOPI */
# include <math.h>

/* note: PI adjusted high */
# define PI		3.1415926536
# define TWOPI		6.2831853071795864769

double
adjlon (lon)
double lon;
{
	while ( fabs(lon) > PI )
		lon += lon < 0. ? TWOPI : -TWOPI;
	return( lon );
}
