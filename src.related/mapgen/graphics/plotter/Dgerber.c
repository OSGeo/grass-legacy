#ifndef lint
static char *SCCSID = "@(#)Dgerber.c	OEMG v.3.2";
#endif
/* Gerber - photohead hi-res plotter */
/* some tape output devices 'swab' bytes.  To counter this
** action define SWAB
*/

#include <varargs.h>
#include "plotter.h"
#include <fcntl.h>

static char s[] = "\0";		/* required structure for return */
static XYS cursor = {0, 0, s};	/* data.  "s" may be larger, if needed */

static int pendown = 2, oldpen = -1;

static int file, SWAB, NL;

# define XPMAX 109700
# define YPMAX 60900
# define RECLEN 72
# define BUFLEN 1*RECLEN
# define TO1000IN(x) (long)((x/2.0)*1.96850393700787401574+0.5) /* coversion for 400cnts/cm */
#define HEADER "*%FSLAX23Y23*OFA0B0*SFA1B1*%"
#define MAXP 24

	static char
cmdbuf[20], outbuf[BUFLEN+2];
	static int
bufp = 0;
	static char *
penmap[] = {
	"10",	/*	w2 line */
	"11",	/*	w4 line */
	"12",	/*	w5 line */
	"13",	/*	w6 line */
	"14",	/*	w7 line */
	"15",	/*	w8 line */
	"16",	/*	w10 line */
	"17",	/*	w12 line */
	"18",	/*	w14 line */
	"19",	/*	w15 line */
	"70",	/*	w16 line */
	"71",	/*	w18 line */
	"20",	/*	w20 line */
	"21",	/*	w25 line */
	"22",	/*	w30 line */
	"23",	/*	w36 line */
	"24",	/*	w40 line */
	"25",	/*	w50 line */
	"26",	/*	w60 line */
	"27",	/*	w75 line */
	"28",	/*	w100 line */
	"29",	/*	w8 line */
	"72",	/*	w16 line */
	"73",	/*	w16 line */
};
	static void
flushp() {
	int n;

	if (bufp) {
		n = bufp % RECLEN;
		if (n) while(n++ < RECLEN)
			outbuf[bufp++] = ' ';
/*		if (NL)
			outbuf[bufp++] = '\n';
		if (SWAB)
			swab(outbuf, outbuf, bufp);
*/
		write(file, outbuf, bufp);
		bufp = 0;
	}
}
	static
putp(str) char *str; {
	int c;

	while (c = *str++) {
		if (bufp >= BUFLEN)
			flushp();
		outbuf[bufp++] = c;
	}
}
	static long
xlast = -1,
ylast = -2;
	XYS *
Dgerber(va_alist) va_dcl {
	va_list vap;
	int cmd, pen;
	long	x, y, t;
	XYS *ret = &cursor;
	static char *spen = "";

	va_start(vap); cmd = va_arg(vap, int);
	switch(cmd) {
	case D_SCALE:
		if (Dglobal.scale <= 0.)
			Dglobal.scale = 1.;
		goto scaleit;
	case D_INIT:
		file = 1;
		SWAB = !(Dglobal.model_no & 1);
		NL = Dglobal.model_no & 2;
		putp(HEADER);
		putp("X0Y0D01");
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
		putp("*X0Y0D02*M02*");
		flushp();
		break;
	case D_MOVE:
		if (pendown) {
			pendown = 0;
			spen = "D02";
		}
		goto moveit;
	case D_LINE:
		if (!pendown) {
			pendown++;
			spen = "D01";
		}
moveit:	
		x = (va_arg(vap, long)) * Dglobal.scale;
		y = (va_arg(vap, long)) * Dglobal.scale;
		if (Dglobal.reverse) {
			t = x;
			x = y;
			y = YPMAX - t;
		}
		x = TO1000IN(x);
		y = TO1000IN(y); 
		putp("*");
		if  (x != xlast) {
			sprintf(cmdbuf,"X%ld", x);
			xlast = x;
			putp(cmdbuf);
		}
		if  (y != ylast) {
			sprintf(cmdbuf,"Y%ld", y);
			ylast = y;
			putp(cmdbuf);
		}
		putp(spen);
		spen = "";
		break;
	case D_PEN:
		pen = (va_arg(vap, long));
		while (pen > MAXP) pen -= MAXP;
		if (pen > 0) pen = pen - 1;
		if (pen != oldpen) {
			oldpen = pen;
			sprintf(cmdbuf,"*G54D%s*G01D01",penmap[pen]);
			putp(cmdbuf);
			pendown = 1;
		}
	}
	va_end(vap);
	return ret;
}
