static char *SCCSID = "@(#)adjlon.c	AMG v.1.1";
/* reduce argument to range +/- PI */
# include <math.h>

/* note: PI adjusted high
** approx. true val:	3.14159265358979323844
*/
# define PI		3.1415926536
# define TWOPI		6.2831853071795864769

	double
adjlon (lon) double lon; {
	while ( fabs(lon) > PI )
		lon += lon < 0. ? TWOPI : -TWOPI;
	return( lon );
}
