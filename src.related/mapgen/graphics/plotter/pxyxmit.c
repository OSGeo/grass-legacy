#ifndef lint
static char *SCCSID = "@(#)pxyxmit.c	USGS v.4.2";
#endif
/* pxyxmit, plotopt */
#include <varargs.h>
#include "graphics.h"

extern BASE base;
	/* line drafting entry */
	void
pxyxmit(mode, x, y) int mode; long x, y; {
	char xs[3], ys[3];
	long xv, yv;
	int nx, ny;

	if (mode & _REL) {
		base.x += (xv = x);
		base.y += (yv = y);
	} else {
		xv = x - base.x;
		yv = y - base.y;
		base.x = x;
		base.y = y;
	}
	nx = vtostr(xv, xs);
	ny = vtostr(yv, ys);
	plotout(_COORD + mode + (nx << 2) + ny);
	while (nx--) plotout(xs[nx]);
	while (ny--) plotout(ys[ny]);
}
	/* option entry */
	void
plotopt(va_alist) va_dcl {
	va_list ap;
	int mode, i, type, n;
	long l;
	char str[3], *s;

	va_start(ap);
	mode = va_arg(ap, int);
	if (mode != P_REQ) {  /* prohibit request code here */
		type = mode & 0xe0;
		switch (type) {
		case _STR:
			plotout(mode);
			s = va_arg(ap, char *);
			do plotout(*s); while (*s++);
			break;
		case _BYTE:
			i = va_arg(ap, int);
			if (i) { plotout(mode); plotout(i);
			} else plotout(mode & _LBMASK);
			break;
		case _NOARG:
			plotout(mode);
			break;
		case _LONG:
			l = va_arg(ap, long);
			if (!(n = vtostr(l, str))) n = 1;
			plotout((mode & _LBMASK)+(n << 5));
			while (n--) plotout(str[n]);
			break;
		default: ; /* error */
		}
	}
	va_end(ap);
}
