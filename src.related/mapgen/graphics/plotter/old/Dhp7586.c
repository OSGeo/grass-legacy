#ifndef lint
static char *SCCSID = "@(#)Dhp7586.c	USGS v.4.1";
#endif
/* hp7586 plotter */
/* the following defines must be included for all drivers */
#include <varargs.h>
#include "plotter.h"

#define file stdout

static char s[] = "\0";		/* required structure for return */
static XYS cursor = {0, 0, s};	/* data.  "s" may be larger, if needed */

static int pendown, oldpen;

static struct {
	long xpmax, ypmax, xoff, yoff;
} model[] = {
	2790,	4500,	-2790,	-4500,
	7100,	4500,	-7100,	-4500,
	7090,	10075, -7090,	-10075,
	15710,	10060, -15710,	-10060,
	20840,	16180, -20840,	-16180
};
# define XPMAX model[Dglobal.model_no].xpmax
# define YPMAX model[Dglobal.model_no].ypmax
# define XOFF model[Dglobal.model_no].xoff
# define YOFF model[Dglobal.model_no].yoff
	XYS *
Dhp7586(va_alist) va_dcl {
	va_list vap;
	int cmd, pen;
	long x, y, t;
	XYS *ret = &cursor;

	va_start(vap); cmd = va_arg(vap, int);
	switch(cmd) {
	case D_SCALE:
		if (Dglobal.scale <= 0.)
			Dglobal.scale = 1.;
		goto scaleit;
	case D_INIT:
		fputs("DF;SP1;PU",file);
		oldpen = pendown = 0;
scaleit:
		if (Dglobal.reverse) {
			cursor.y = XPMAX / Dglobal.scale;
			cursor.x = YPMAX / Dglobal.scale;
		} else {
			cursor.x = XPMAX / Dglobal.scale;
			cursor.y = YPMAX / Dglobal.scale;
		}
		break;
	case D_DONE:
		fputs(";SP0;",file);
		break;
	case D_MOVE:
		if (pendown) {
			pendown = 0;
			fputs(";PU", file);
		}
		goto moveit;
	case D_LINE:
		if (!pendown) {
			pendown++;
			fputs(";PD", file);
		}
moveit:	
		x = (va_arg(vap, long)) * Dglobal.scale;
		y = (va_arg(vap, long)) * Dglobal.scale;
		if (Dglobal.reverse) {
			t = x;
			x = y;
			y = YPMAX - t;
		}
		fprintf(file,"%d,%d,",(x << 1) + XOFF,(y << 1) + YOFF);
		break;
	case D_PEN:
		pen = (va_arg(vap, long)) & 7; /* 8 pens */
		if (pen != oldpen) {
			oldpen = pen;
			fprintf(file,";SP%d;PU",pen+1);
			pendown = 0;
		}
	}
	va_end(vap);
	return ret;
}
