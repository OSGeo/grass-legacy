#ifndef lint
static char *SCCSID = "@(#)urequest.c	USGS v.4.2";
#endif
/* return to applications process requested data */
# define PLOTTER
# include "graphics.h"
# include "plotter.h"

extern PEN *pen;

extern int error;

extern BASE base;

extern long base_x, base_y;

static FILE *out;

extern XYS *(*device)();

static void xyxmit(), optout();

urequest(v) long v; {
	FILE *ufile();
	long x, y;
	XYS *pxy;

	if ((out = ufile()) == NULL)
		return;
	v &= 255;
	switch ((int)v) {
	case ERROR:		/* return current error code */
		optout(_LONG, (long)error);
		error = 0;
		break;
	case P_SIZE:		/* return plotter size */
		max_x_y(&x, &y);
		xyxmit(x, y);
		break;
	case CURSOR:		/* get and return cursor position */
		if ((pxy = (*device)(D_CURSOR)) != NULL) {
			xyxmit(pxy->x - base_x, pxy->y - base_y);
			optout(_STR, (long)pxy->s);
		} else {
			error = E_NOCURSOR;
			optout(_LONG, (long)error);
		}
		break;
	case FSIZE:		/* return current font basic size */
		if (pen && pen->font)
			optout(_LONG, (long)pen->font->vect[0]);
		else
			optout(_LONG, (long)0);
		break;
	case FSSIZE:		/* return current symbol font basic size */
		if (pen && pen->sfont)
			optout(_LONG, (long)pen->sfont->vect[0]);
		else
			optout(_LONG, (long)0);
		break;
	default:
		error = E_BADRQ;
	}
	fputc(P_ACK, out);	/* acknowledgement byte */
	fflush(out);
}
	static void /* transmit x-y */
xyxmit(x, y) long x, y; {
	char xs[3], ys[3];

	x = vtostr(x, xs);
	y = vtostr(y, ys);
	fputc((int)(_COORD + (x << 2) + y), out);
	while (x--) fputc(xs[x], out);
	while (y--) fputc(ys[y], out);
}
	static void /* option entry */
optout(mode, arg) int mode; long arg; {
	char str[3];
	int type, n;

	type = mode & 0xe0;
	switch (type) {
	case _STR:
		fputc(mode, out);
		fputs((char *)arg, out);
		fputc('\0', out);
		break;
	case _BYTE:
		if (arg) {
			fputc(mode, out);
			fputc(arg, out);
		} else
			fputc(mode & _LBMASK, out);
		break;
	case _LONG:
		if (!(n = vtostr((long)arg, str)))
			n = 1;
		fputc((mode & _LBMASK)+(n << 5), out);
		while (n--) fputc(str[n], out);
		break;
	}
}
