#ifndef lint
static char *SCCSID = "@(#)Dkong.c	OEMG v.3.1";
#endif
/* Kongsberg - photohead hi-res plotter */
/* some tape output devices 'swab' bytes.  To counter this
** action define SWAB */

#include <varargs.h>
#include "plotter.h"
#include <fcntl.h>

# define SCRIBE (Dglobal.model_no & 1)
# define SWAB (Dglobal.model_no & 2)

static char s[] = "\0";		/* required structure for return */
static XYS cursor = {0, 0, s};	/* data.  "s" may be larger, if needed */

static int pendown, oldpen;

static int file;

# define XPMAX 24000
# define YPMAX 32000
	static char
cmdbuf[20], outbuf[512];
	static int
bufp = 0;
	static char * /* based on M21,22,23 equiv. to x1, x2, x3 */
penmap[] = {
	"D11@M21@",	/*   2 micron, pen:  0 */
	"D12@M21@",	/*   3 micron, pen:  1 */
	"D11@M22@",	/*   4 micron, pen:  2 */
	"D13@M21@",	/*   5 micron, pen:  3 */
	"D11@M23@",	/*   6 micron, pen:  4 */
	"D14@M21@",	/*   7 micron, pen:  5 */
	"D12@M23@",	/*   9 micron, pen:  6 */
	"D13@M22@",	/*  10 micron, pen:  7 */
	"D14@M22@",	/*  14 micron, pen:  8 */
	"D13@M23@",	/*  15 micron, pen:  9 */
	"D14@M23@",	/*  21 micron, pen: 10 */
	"D76@M21@",	/*  30 micron, pen: 11 */
	"D77@M21@",	/*  45 micron, pen: 12 */
	"D76@M22@",	/*  60 micron, pen: 13 */
	"D76@M23@",	/*  90 micron, pen: 14 */
	"D77@M23@"	/* 135 micron, pen: 15 */
};
	static void
flushp() {
	if (bufp) {
		while (bufp < 512)
			outbuf[bufp++] = ' ';
		if (SWAB) swab(outbuf, outbuf, 512);
		write(file, outbuf, 512);
		bufp = 0;
	}
}
	static
putp(str) char *str; {
	int n, t;

	t = bufp + (n = strlen(str));
	if (t > 512)
		flushp();
	else if (((t = bufp & 077) + n ) > 64)
		while (t++ < 64)
			outbuf[bufp++] = ' ';
	while (n--)
		outbuf[bufp++] = *str++;
}
	XYS *
Dkong(va_alist) va_dcl {
	va_list vap;
	long	x, y, t;
	int	cmd, pen;
	static char *spen = "@";
	XYS *ret = &cursor;

	va_start(vap); cmd = va_arg(vap, int);
	switch(cmd) {
	case D_SCALE:
		if (Dglobal.scale <= 0.)
			Dglobal.scale = 1.;
		goto scaleit;
	case D_INIT:
		file = 1;
		putp(SCRIBE ? "G54@M37@G1@X0Y0M59@D2@D10@" :
			"G55@M37@G1@M21@X0Y0M59@D2@D11@" );
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
	case D_PANIC:
		putp("X0Y0D2@M00@M02@");
		flushp();
		close(1);
		break;
	case D_MOVE:
		if (pendown) {
			pendown = 0;
			spen = "D2@";
		}
		goto moveit;
	case D_LINE:
		if (!pendown) {
			pendown++;
			spen = "D1@";
		}
moveit:	
		x = (va_arg(vap, long)) * Dglobal.scale;
		y = (va_arg(vap, long)) * Dglobal.scale;
		if (Dglobal.reverse) {
			t = x;
			x = y;
			y = YPMAX - t;
		}
		sprintf(cmdbuf, "X%dY%d%s", x * 5, y * 5, spen);
		putp(cmdbuf);
		spen = "@";
		break;
	case D_PEN:
		if (! SCRIBE) { /* ignore with scribe pen */
			pen = (va_arg(vap, long)) & 0xf; /* 16 pens */
			if (pen != oldpen) {
				oldpen = pen;
				sprintf(cmdbuf,"%s%s",pendown?"D2@":"",
					penmap[pen]);
				putp(cmdbuf);
				pendown = 0;
			}
		}
		break;
	}
	va_end(vap);
	return ret;
}
