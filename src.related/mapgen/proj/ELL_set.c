#ifndef lint
static char *SCCSID = "@(#)ELL_set.c	USGS v.3.1";
#endif
# include "projects.h"
/* We need to use a default value which will format
** without conversion problems. HUGE cannot be used
** in this case. But TAG and STAG should work for most
** machines and be sufficiently wierd so that it is not
** a likely user entry.
*/
# define TAG	1e14
# define STAG	"1e14"
	void /* initialize ELLIPSE structure */
ELL_set() {
	double b;

		/* check for varying forms of eccentricity input */
	es = b = 0.;
			/* eccentricity squared */
	if ((es = *(*param)("des", STAG)) == TAG) {
			/* eccentricity */
		if ((es = *(*param)("de", STAG)) != TAG)
			es = es * es;
			/* reciprocal flattening */
		else if ((es = *(*param)("drf", STAG)) != TAG) {
			if (!es)
				emess(1,"reciprocal flattening = 0");
			es = 1./es;
			es = es * (2. - es);
			/* flattening */
		} else if ((es = *(*param)("df", STAG)) != TAG)
			es = es * (2. - es);
			/* minor axis */
		else
			b = *(*param)("db", "");
	}
	if (!(a = *param("da","")))
		emess(1,"major axis or radius = 0");
	if (b) /* es from minor axis input */
		es = 1. - (b * b) / (a * a);
	else if (es == TAG)
		es = 0.;
		/* set remainder of ellipsoid structure */
	e = sqrt(es);
	ra = 1./a;
	one_es = 1. - es;
	rone_es = 1./one_es;
		/* central meridian */
	lam0 = *(*param)("rlon_0", "");
	phi0 = *(*param)("rlat_0", "");
		/* false easting and northing */
	x0 = *(*param)("dx_0", "");
	y0 = *(*param)("dy_0", "");
}
