static char *SCCSID = "@(#)msfn_.c	AMG v.1.1";
/* determine constant small m */
# include <math.h>

	double
msfn_ (eccent,sinphi,cosphi) double eccent, sinphi, cosphi; {
	eccent *= sinphi;
	return (cosphi / sqrt (1. - eccent * eccent));
}
