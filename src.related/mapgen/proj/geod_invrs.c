#ifndef lint
static char *SCCSID = "@(#)geod_invrs.c	USGS v.1.1";
#endif
# include "projects.h"
# include "geodesic.h"
# define DTOL	1e-12
	void
geod_invrs() {
	double	th1,th2,thm,dthm,dlamm,dlam,sindlamm,costhm,sinthm,cosdthm,
		sindthm,L,E,cosd,d,X,Y,T,sind,tandlammp,u,v,D,A,B;

	if (ellipse) {
		th1 = atan(onef * tan(phi1));
		th2 = atan(onef * tan(phi2));
	} else {
		th1 = phi1;
		th2 = phi2;
	}
	thm = .5 * (th1 + th2);
	dthm = .5 * (th2 - th1);
	dlamm = .5 * ( dlam = adjlon(lam2 - lam1) );
	if (fabs(dlam) < DTOL && fabs(dthm) < DTOL) {
		al12 =  al21 = S = 0.;
		return;
	}
	sindlamm = sin(dlamm);
	costhm = cos(thm);	sinthm = sin(thm);
	cosdthm = cos(dthm);	sindthm = sin(dthm);
	if (ellipse) {
		L = sindthm * sindthm + (cosdthm * cosdthm - sinthm * sinthm)
			* sindlamm * sindlamm;
		E = 2. * (cosd = 1 - L - L);
		sind = sin( d = acos(cosd) );
		Y = sinthm * cosdthm;
		Y *= (Y + Y) / (1. - L);
		T = sindthm * costhm;
		T *= (T + T) / L;
		X = Y + T;
		Y -= T;
		T = d / sind;
		D = 4. * T * T;
		A = D * E;
		B = D + D;
		S = a * sind * (T - f4 * (T * X - Y) +
			f64 * (X * (A + (T - .5 * (A - E)) * X) -
			Y * (B + E * Y) + D * X * Y));
		tandlammp = tan(.5 * (dlam - .25 * (Y + Y - E * (4. - X)) *
			(f2 * T + f64 * (32. * T - (20. * T - A)
			* X - (B + 4.) * Y)) * tan(dlam)));
	} else {
		S = a * d;
		tandlammp = tan(dlamm);
	}
	u = atan2(sindthm , (tandlammp * costhm));
	v = atan2(cosdthm , (tandlammp * sinthm));
	al12 = adjlon(TWOPI + v - u);
	al21 = adjlon(TWOPI - v - u);
}
