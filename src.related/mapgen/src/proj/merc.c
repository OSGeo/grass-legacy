static char *SCCSID = "@(#)merc.c	AMG v.1.1";
/* Mercator projection */
# define	PROJ_MOD
# include	"projects.h"
struct _PROJ { PHEAD;
	double PHITS, AP, E;
};
# define EPS10 1.e-10
# define phits	proj->PHITS
# define ap	proj->AP
FORWARD(e_forward); /* ellipsoid */
	if (fabs(fabs(phi) - HALFPI) <= EPS10) ERROR;
	x = ap * lam;
	y = - ap * log(tsfn_(e, phi, sin(phi)));
	return (&xy);
}
INVERSE(e_inverse); /* ellipsoid */
	if ((phi = phi2_(e, exp(- y / ap))) == HUGE) ERROR;
	lam = x / ap;
	return (&lp);
}
FORWARD(s_forward); /* spheroid */
	if (fabs(fabs(phi) - HALFPI) <= EPS10) ERROR;
	x = ap * lam;
	y = ap * log(tan(FORTPI + .5 * phi));
	return (&xy);
}
INVERSE(s_inverse); /* spheroid */
	phi = HALFPI - 2. * atan(exp(-y / ap));
	lam = x / ap;
	return (&lp);
}
ENTRY(merc) {
	if (! proj) return (sizeof(struct _PROJ));
	if (comset(I_ALL, proj, param)) ERROR;
	phits = fabs(*(*param)("rlat_ts", ""));
	if (es) { /* ellipsoid */
		proj->forward = e_forward;
		proj->inverse = e_inverse;
		e = sqrt(es);
		ap = msfn_(e, sin(phits), cos(phits));
	} else { /* sphere */
		proj->forward = s_forward;
		proj->inverse = s_inverse;
		ap = cos(phits);
	}
	return (1);
}
