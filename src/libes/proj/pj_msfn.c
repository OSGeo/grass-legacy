/* determine constant small m */
#ifndef lint
static char RCSID[] = "@(#)$Id: pj_msfn.c,v 4.1 1992/04/03 13:35:37 gie Exp $";
#endif
#include <math.h>

double
#ifdef __STDC__
pj_msfn(double sinphi, double cosphi, double es)
#else
pj_msfn(sinphi, cosphi, es)
    double sinphi, cosphi, es;
#endif
{
	return (cosphi / sqrt (1. - es * sinphi * sinphi));
}
