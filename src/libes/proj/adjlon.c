/* reduce argument to range +/- PI */
#ifndef lint
static char RCSID[] = "@(#)$Id: adjlon.c,v 4.1 1992/04/03 13:35:40 gie Exp $";
#endif
#include <math.h>
/* note: PI adjusted high
** approx. true val:	3.14159265358979323844
*/
#define SPI		3.14159265359
#define TWOPI	6.2831853071795864769

double
#ifdef __STDC__
adjlon (double lon)
#else
adjlon (lon)
    double lon;
#endif
{
	while ( fabs(lon) > SPI )
		lon += lon < 0. ? TWOPI : -TWOPI;
	return( lon );
}
