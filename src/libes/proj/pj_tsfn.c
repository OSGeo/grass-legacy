/* determine small t */
#ifndef lint
static char RCSID[] = "@(#)$Id: pj_tsfn.c,v 4.1 1992/04/03 13:35:39 gie Exp $";
#endif
#include <math.h>
#define HALFPI		1.5707963267948966
	double
#ifdef __STDC__
pj_tsfn(double phi, double sinphi, double e)
#else
pj_tsfn(phi, sinphi, e)
    double phi, sinphi, e;
#endif
{
	sinphi *= e;
	return (tan (.5 * (HALFPI - phi)) /
	   pow((1. - sinphi) / (1. + sinphi), .5 * e));
}
