static char *SCCSID = "@(#)eqc.c	AMG v.1.1";
/* Equidistant Cylindrical projection (Plate Caree) */
# define	PROJ_MOD
# include	"projects.h"
struct _PROJ { PHEAD;
	double PHI1, RC;
};
# define phi1	proj->PHI1
# define rc	proj->RC
FORWARD(s_forward); /* spheroid */
	x = rc * lam;
	y = phi;
	return (&xy);
}
INVERSE(s_inverse); /* spheroid */
	phi = y;
	lam = x / rc;
	return (&lp);
}
ENTRY(eqc) {
	if (! proj) return (sizeof(struct _PROJ));
	if (comset(I_LAM0 + I_XY, proj, param)) ERROR;
	phi1 = *(*param)("rlat_ts", "");
	proj->forward = s_forward;
	proj->inverse = s_inverse;
	return ((rc = cos(phi1)) != 0.);
}
