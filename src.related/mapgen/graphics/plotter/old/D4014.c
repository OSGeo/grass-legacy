#ifndef lint
static char *SCCSID = "@(#)D4014.c	OEMG v.3.1";
#endif
/* generic Tektronics 4014/4010 driver for program "plotter" */
#include <varargs.h>
#include "plotter.h"

static char s[] = "\0";		/* required structure for return */
static XYS cursor = {0, 0, s};	/* data.  "s" may be larger, if needed */

/* the remainder of this code is purely device dependent */
# define HIYTAG 0x20
# define HIXTAG 0x20
# define EXTAG  0x60
# define LOYTAG 0x60
# define LOXTAG 0x40
# define XYMASK 0x1f
# define GS 29
# define US 31
	/* list of prelude - postlude strings */
static struct {
	char	*prelude, *postlude;	/* pre/post graphics enable strings */
	int	f4014;		/* 4014 type device */
	int	delay;		/* delay upon clearing screen (old models) */
	int	no_gin;		/* number of cursor input characters */
	char	*d_on, *d_off;	/* draw / no_draw vect. (later for erase) */
} model [] = {
	"",	"", 0, 1, 6, "","",	/* native 401x mode */
	"",	"", 1, 1, 6, "","",	/* native 4014 mode */
	"\0331", "\0332", 0, 0, 8, "\033\001", "\033\020", /* GraphOn */
	"\035", "\033\"0g", 0, 0, 8, "","", /* tab terminal */
	"\033[2+z", "\030", 0, 0, 8, "","" /* hds terminal */
};
	/* basic screen ranges */
# define XPMAX4 4095
# define YPMAX4 3120
# define XPMAX0 1023
# define YPMAX0 781
# define XPMAX	(f_t4014 ? XPMAX4 : XPMAX0)
# define YPMAX	(f_t4014 ? YPMAX4 : YPMAX0)
	static int
ex_last = -1,
lox_last = -1,
hix_last = -1,
hiy_last = -1,
loy_last = -1,
enabled = 0,
f_t4014;

	static
enable() {
	if (!enabled) {
		fputs(model[Dglobal.model_no].prelude, stdout);
		fputc(GS, stdout);
		if (lox_last != -1)
			fputc(lox_last + LOXTAG, stdout);
		enabled = 1;
	}
}
	XYS *
D4014(va_alist) va_dcl {
	va_list vap;
	int cmd, hix, hiy, ex, x, y, xv, yv;
	char curbuf[16];
	XYS *ret = &cursor;

	va_start(vap); cmd = va_arg(vap, int);
	switch(cmd) {
	case D_SCALE:
		if (Dglobal.scale <= 0.)
			Dglobal.scale = 1.;
		goto scaleit;
	case D_INIT:
		f_t4014 = model[Dglobal.model_no].f4014;
		enabled = 0;
scaleit:
		if (Dglobal.reverse) { cursor.x = YPMAX; cursor.y = XPMAX; }
		else		{ cursor.y = YPMAX; cursor.x = XPMAX; }
		cursor.x = cursor.x / Dglobal.scale;
		cursor.y = cursor.y / Dglobal.scale;
		break;
	case D_DONE:
		if (enabled) {
			fflush(stdin);
			if (!Dglobal.quiet) {
				fputc(US, stdout); fputc(7, stdout);
				fflush(stdout);
				getc(stdin);
			}
		}
	case D_PANIC:
		if (enabled)
			fputs(model[Dglobal.model_no].postlude, stdout);
		break;
	case D_DISABL:
		if (enabled) {
			fputc(31, stdout);
			fputs(model[Dglobal.model_no].postlude, stdout);
			fflush(stdout);
			enabled = 0;
		}
		break;
	case D_MOVE:
		enable();
		fputc(GS, stdout);
		ex_last = lox_last = hix_last = hiy_last = loy_last = -1;
		goto moveit;
	case D_LINE:
		enable();
moveit:
		xv = va_arg(vap, long);
		yv = va_arg(vap, long);
		if (Dglobal.reverse) {
			y = xv * Dglobal.scale + .5;
			x = XPMAX - (long)(yv * Dglobal.scale + .5);
		} else {
			y = yv * Dglobal.scale + .5;
			x = xv * Dglobal.scale + .5;
		}
		if (f_t4014) {
			ex = ((y & 3) << 2) | (x & 3);
			hix = (x >> 7) & XYMASK;
			hiy = (y >> 7) & XYMASK;
			x = (x >> 2) & XYMASK;
			y = (y >> 2) & XYMASK;
		} else {
			hix = (x >> 5) & XYMASK;
			hiy = (y >> 5) & XYMASK;
			x &= XYMASK;
			y &= XYMASK;
		}
		if (hiy != hiy_last)
			fputc((hiy_last = hiy) + HIYTAG, stdout);
		if (f_t4014 && ex != ex_last) {
			fputc((ex_last = ex) + EXTAG, stdout);
			goto low_y;
		}
		if (hix != hix_last || y != loy_last) {
low_y:			fputc((loy_last = y) + LOYTAG, stdout);
			if (hix != hix_last)
				fputc((hix_last = hix) + HIXTAG, stdout);
		}
		fputc(x + LOXTAG, stdout);
		lox_last = x;
		break;
	case D_ERASE:
		enable();
		fputs("\033\014", stdout);
		fflush(stdout);
		if (model[Dglobal.model_no].delay);
			sleep(2);
		break;
	case D_PEN:
		fputs((va_arg(vap, long)) & 0x80 ?
			model[Dglobal.model_no].d_off :
			model[Dglobal.model_no].d_on,
			stdout);
		break;
	case D_CURSOR:
		enable();
		fflush(stdin);
		fputs("\033\032", stdout);
		fflush(stdout);
		fgets(curbuf, model[Dglobal.model_no].no_gin, stdin);
		fputc(GS, stdout);
		if (lox_last != -1)
			fputc(lox_last + LOXTAG, stdout);
		*cursor.s = *curbuf & 0x7f;
		cursor.x = ((curbuf[1] & XYMASK) << 5) +
			(curbuf[2] & XYMASK);
		cursor.y = ((curbuf[3] & XYMASK) << 5) +
			(curbuf[4] & XYMASK);
		if (f_t4014) {
			cursor.x <<= 2;  /* adjust for 4014 */
			cursor.y <<= 2;
		}
		if (Dglobal.reverse) {
			static long temp;
			temp = cursor.y;
			cursor.y = XPMAX - cursor.x;
			cursor.x = temp;
		}
		cursor.x = cursor.x / Dglobal.scale + .5;
		cursor.y = cursor.y / Dglobal.scale + .5;
		break;
	default:
		break;
	}
	return ret;
}
