static char *SCCSID = "@(#)comset.c	AMG v.1.1";
/* basic projection user link and initializer */
# define	PROJ_MOD
# include	"projects.h"

/* standard base of all projection structures */
struct _PROJ { PHEAD; };

/* We need to use a default value which will format
** without conversion problems. HUGE cannot be used
** in this case. But TAG and STAG should work for most
** machines and be sufficiently wierd so that it is not
** a likely user entry.
*/
# define TAG	1e14
# define STAG	"1e14"

/* projection basic parameter initializer entry */
comset(n, proj, param)
int n;
struct _PROJ *proj;
double *(*param)();
{
	double b;

		/* check for varying forms of eccentricity input */
	es = b = 0.;
			/* eccentricity squared */
	if ((n & I_ELLPS) && (es = *(*param)("des", STAG)) == TAG) {
			/* eccentricity */
		if ((es = *(*param)("de", STAG)) != TAG)
			es = es * es;
			/* reciprocal flattening */
		else if ((es = *(*param)("drf", STAG)) != TAG) {
			es = 1./es;
			es = es * (2. - es);
			/* flattening */
		} else if ((es = *(*param)("df", STAG)) != TAG)
			es = es * (2. - es);
			/* minor axis */
		else
			b = *(*param)("db", "");
	}
	if ((a = *(*param)("da", STAG)) == TAG) {
		/* to provide easy access to the default
		** spherical value, es=0 will be allowed */
		if (n & I_ELLPS && es != 0.) {
			if (es != TAG || b)
				/* no major axis value */
				return (1);
			a = A_ELLIPS; /* default ellipse */
			es = ES_ELLIPS;
		} else
			a = A_SPHERE; /* default sphere */
	}
	if (b) /* es from minor axis input */
		es = 1. - (b * b) / (a * a);
	else if (es == TAG)
		es = 0.;
	if (n & I_LAM0) /* central meridian */
		lam0 = *(*param)("rlon_0", "");
	if (n & I_XY) { /* false easting and northing */
		x0 = *(*param)("dx_0", "");
		y0 = *(*param)("dy_0", "");
	}
		/* return check */
	return (! (a > 0. && es >= 0.));
}

# define EPS 1.0e-12

/* forward projection entry */
	UV *
forwd(lp, proj) UV lp; struct _PROJ *proj; {
	UV *xy;
	double t;
	int err;

	/* check for latitude or longitude overange */
	if ((err = ((t = fabs(lp.v)) >= HALFPI)) &&
	   ! (err = ((t - EPS) > HALFPI))) /* grossly over? */
		lp.v = lp.v < 0. ? -HALFPI : HALFPI;
	if (err || (fabs(lp.u) - EPS) > TWOPI) /* accpt. +/- 2.*PI */
		ERROR;
	else { /* checks OK at this level */
		lp.u = adjlon(lp.u - lam0); /* adjust longitude */
		if (xy = (UV *)(*proj->forward)(lp, proj)) { /* project */
			xy->u = a * xy->u + x0;
			xy->v = a * xy->v + y0;
		}
	}
	return xy;
}

/* inverse projection entry */
	UV *
invrs(xy, proj) UV xy; struct _PROJ *proj; {
	UV *lp;

	/* can't do preliminary checking as with forward */
	xy.u = (xy.u - x0) / a; /* descale and de-offset */
	xy.v = (xy.v - y0) / a;
	if (lp = (UV *)(*proj->inverse)(xy, proj)) /* inverse project */
		lp->u = adjlon(lp->u + lam0); /* adjust longitude to CM */
	return lp;
}
