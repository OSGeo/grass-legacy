#ifndef lint
static char *SCCSID = "@(#)setsizes.c	USGS v.4.1";
#endif
/* set string and symbol size and rotations */
# define PLOTTER
# include "graphics.h"
# include "plotter.h"
	extern PEN *
pen;
	extern int
error;
	/* set character attributes */
setstrsz(cmd, v) int cmd; long v; {
	double t, sin(), cos();

	switch (cmd) {
	caseof(SIZE):
		pen->csize = v > 0 ? v << 4 : -v;
		break;
	caseof(ANG):
		if (v) {
			pen->cost = cos(t = v / _ANGCVT);
			pen->sint = sin(t);
			pen->cflags |= C_THETA;
		}
		else
			pen->cflags &= ~C_THETA;
		break;
	caseof(XOFF):
		pen->xoff = v;
		break;
	caseof(YOFF):
		pen->yoff = v;
		break;
	}
	if (pen->cflags & C_THETA) {
		pen->bcost = .0625 * pen->csize * pen->cost;
		pen->bsint = .0625 * pen->csize * pen->sint;
		pen->r_xoff = pen->xoff * pen->cost -
			 pen->yoff * pen->sint;
		pen->r_yoff = pen->yoff * pen->cost +
			 pen->xoff * pen->sint;
	}
	else {
		pen->r_xoff = pen->xoff;
		pen->r_yoff = pen->yoff;
	}
	return(0);
}
	/* set symbol attributes */
setsymsz(cmd, v) int cmd; long v; {
	double t, sin(), cos();

	if ((SSIZE & _LBMASK) == cmd) /* set symbol size */
		pen->ssize = v > 0 ? v << 4 : -v;
	else if (v) {  /* set symbol rotation */
		pen->scost = cos(t = v / _ANGCVT);
		pen->ssint = sin(t);
		pen->cflags |= C_STHETA;
	} else
		pen->cflags &= ~C_STHETA;
	if (pen->cflags & C_STHETA) {
		pen->sbcost = .0625 * pen->ssize * pen->scost;
		pen->sbsint = .0625 * pen->ssize * pen->ssint;
	}
	return(0);
}
