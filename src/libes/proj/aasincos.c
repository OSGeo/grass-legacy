/* arc sin and cosine that will not fail */
#ifndef lint
static char RCSID[] = "@(#)$Id: aasincos.c,v 4.3 1992/05/27 14:40:13 gie Exp $";
#endif
#include <math.h>
#include "projects.h"

double
#ifdef __STDC__
aasin(double v)
#else
aasin(v)
    double v;
#endif
{
	if (fabs(v) >= 1.)
		return (v < 0. ? -HALFPI : HALFPI);
	return asin(v);
}

double
#ifdef __STDC__
aacos(double v)
#else
aacos(v)
    double v;
#endif
{
	if (fabs(v) >= 1.)
		return 0.;
	return acos(v);
}
double
#ifdef __STDC__
asqrt(double v)
#else
asqrt(v)
    double v;
#endif
{ return ((v <= 0) ? 0. : sqrt(v)); }
