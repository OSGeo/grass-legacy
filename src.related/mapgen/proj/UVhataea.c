#ifndef lint
static char *SCCSID = "@(#)UVhataea.c	USGS v.3.1";
#endif
/*  Hatano Asymmetrical Equal Area */
# include	"projects.h"
# define NITER	20
# define EPS	1e-7
# define ONETOL 1.000001
# define CN	2.67595
# define CS	2.43763
# define RCN	0.37369906014686373063
# define RCS	0.41023453108141924738
# define FYCN	1.75859
# define FYCS	1.93052
# define RYCN	0.56863737426006061674
# define RYCS	0.51799515156538134803
# define FXC	0.85
# define RXC	1.17647058823529411764
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	double th1, c;
	int i;

	c = sin(phi) * (phi < 0. ? CS : CN);
	for (i = NITER; i; --i) {
		phi -= th1 = (phi + sin(phi) - c) / (1. + cos(phi));
		if (fabs(th1) < EPS) break;
	}
	x = FXC * lam * cos(phi *= .5);
	y = sin(phi) * (phi < 0. ? FYCS : FYCN);
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	double th;

	th = y * ( y < 0. ? RYCS : RYCN);
	if (fabs(th) > 1.)
		if (fabs(th) > ONETOL)	I_ERROR
		else			th = th > 0. ? HALFPI : - HALFPI;
	else
		th = asin(th);
	lam = RXC * x / cos(th);
	th += th;
	phi = (th + sin(th)) * (y < 0. ? RCS : RCN);
	if (fabs(phi) > 1.)
		if (fabs(phi) > ONETOL)	I_ERROR
		else			phi = phi > 0. ? HALFPI : - HALFPI;
	else
		phi = asin(phi);
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(hataea) { if (inverse) RETURN(s_inverse); else RETURN(s_forward); }
