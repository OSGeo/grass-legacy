#ifndef lint
static char *SCCSID = "@(#)geod_forwd.c	USGS v.1.1";
#endif
# include "projects.h"
# include "geodesic.h"
# define MERI_TOL 1e-9
	static double
th1,costh1,sinth1,sina12,cosa12,M,N,c1,c2,D,P,s1;
	static int
merid;
	void
geod_prefor() {
	th1 = ellipse ? atan(onef * tan(phi1)) : phi1;
	al12 = adjlon(al12); /* reduce to  +- 0-PI */
	costh1 = cos(th1);
	sinth1 = sin(th1);
	if ((merid = fabs(sina12 = sin(fabs(al12))) < MERI_TOL)) {
		sina12 = 0.;
		cosa12 = al12 < 1. ? 1. : -1.;
		M = 0.;
		N = costh1;
	} else {
		cosa12 = cos(al12);
		M = costh1 * sina12;
		N = costh1 * cosa12;
	}
	if (ellipse) {
		if (merid) {
			c1 = 0.;
			c2 = f4;
			D = 1. - c2;
			D *= D;
			P = c2 / D;
		} else {
			c1 = f * M;
			c2 = f4 * (1. - M * M);
			D = (1. - c2)*(1. - c2 - c1 * M);
			P = (1. + .5 * c1 * M) * c2 / D;
		}
	}
	if (merid) s1 = HALFPI - th1;
	else	/* may be slight overrun (>1.0---x) */
		if (fabs(fabs(M) - 1.) < 1e-13 || fabs(sinth1) < 1e-13)
			s1 = HALFPI;
		else {
			s1 =  sinth1 / sin(acos(M));
			if (fabs(s1) > 1.)
				s1 = s1 > 0. ? HALFPI : -HALFPI;
			else
				s1 = acos(sinth1/sin(acos(M)));
		}
}
	void
geod_forwd() {
	double d,sind,u,V,X,ds,cosds,sinds,ss,de;

	if (ellipse) {
		d = S / (D * a);
		u = 2. * (s1 - d);
		V = cos(u + d);
		X = c2 * c2 * (sind = sin(d)) * cos(d) * (2. * V * V - 1.);
		ds = d + X - 2. * P * V * (1. - 2. * P * cos(u)) * sind;
		cosds = cos(ds);
		sinds = sin(ds);
		ss = s1 + s1 - ds;
	} else {
		d = S / a;
		cosds = cos(d);
		sinds = sin(d);
	}
	al21 = N * cosds - sinth1 * sinds;
	if (merid) {
		phi2 = atan(tan(HALFPI + s1 - ds) / onef);
		if (al21> 0.) {
			al21 = PI;
			de = 0.;
			phi2 = - phi2;
		} else {
			al21 = TWOPI;
			de = PI;
		}
	} else {
		al21 = adjlon(PI + atan2(M, al21));
		if (al12 < 0.)
			al21 = -al21;
		phi2 = atan2(-(sinth1 * cosds + N * sinds) * sin(al21),
			ellipse ? onef * M : M);
		de = atan2(sinds * sina12 ,
			(costh1 * cosds - sinth1 * sinds * cosa12));
	}
	if (ellipse)
		de -= c1 * ((1. - c2) * ds - c2 * sinds * cos(ss));
	lam2 = adjlon( lam1 + (al12 >= 0 ? de : -de) );
}
