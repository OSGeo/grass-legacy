#ifndef lint
static char *SCCSID = "@(#)Dranger.c	USGS v.4.1";
#endif
/* range determination non-plotter driver */
#include <varargs.h>
#include "plotter.h"

static char s[2];		/* required structure for return */
static XYS cursor = {0, 0, s};	/* data.  "s" may be larger, if needed */

# define XPMAX 100000L
# define YPMAX 100000L
	static long
xmin, ymin, xmax, ymax;
	static int
first;
	XYS *
Dranger(va_alist) va_dcl {
	va_list vap;
	int cmd;
	long xv, yv;
	XYS *ret = &cursor;

	va_start(vap); cmd = va_arg(vap, int);
	switch(cmd) {
	case D_SCALE:
		if (Dglobal.scale <= 0.)
			Dglobal.scale = 1.;
		goto scaleit;
	case D_INIT:
scaleit:
		cursor.x = XPMAX / Dglobal.scale;
		cursor.y = XPMAX / Dglobal.scale;
		first = 1;
		break;
	case D_DONE:
	case D_PANIC:
		(void)printf("%ld\t%ld\t%ld\t%ld\n", xmin, xmax, ymin, ymax);
		break;
	case D_MOVE:
	case D_LINE:
		xv = va_arg(vap, long);
		yv = va_arg(vap, long);
		if (first) {
			xmin = xmax = xv;
			ymin = ymax = yv;
			first = 0;
		} else {
			if (xv < xmin)
				xmin = xv;
			else if (xv  > xmax)
				xmax = xv;
			if (yv < ymin)
				ymin = yv;
			else if (yv  > ymax)
				ymax = yv;
		}
		break;
	}
	va_end(vap);
	return (ret);
}
