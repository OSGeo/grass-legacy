static char *SCCSID = "@(#)mill.c	AMG v.1.1";
/* Miller Cylindrical projection */
# define	PROJ_MOD
# include	"projects.h"
struct _PROJ { PHEAD; };
FORWARD(s_forward); /* spheroid */
	x = lam;
	y = log(tan(FORTPI + phi * .4)) * 1.25;
	return (&xy);
}
INVERSE(s_inverse); /* spheroid */
	lam = x;
	phi = 2.5 * (atan(exp(.8 * y)) - FORTPI);
	return (&lp);
}
ENTRY(mill) {
	if (! proj) return (sizeof(struct _PROJ));
	if (comset(I_LAM0 + I_XY, proj, param)) ERROR;
	proj->forward = s_forward;
	proj->inverse = s_inverse;
	return (1);
}
