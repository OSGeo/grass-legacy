#ifndef lint
static char *SCCSID = "@(#)Dc970.c	USGS v.4.1";
#endif
/* Calcomp 970 plotter */
# include <varargs.h>
# include "plotter.h"

#define file stdout
/* external option control */

# define MAX_BUF	16
# define SMASK		0x20
# define LMASK		0x40
# define PENCON		0x02
# define PENMASK	0x70
# define PENUP		0x00
# define PENDOWN	0x01
# define PENOFF		1219
# define INITLEN	3

/* There are several different bed sizes for the 970 so check
** the dimensions on your system.  The following numbers
** come from 800 * size_in_centimeters */
/* 200 cm. in X axis */
# define XPMAX		160000.
/* 130 cm. in Y axis */
# define YPMAX		104160.
	static
union {
	struct {
		char x, y;
	} sbuf[MAX_BUF];
	struct {
		short x, y;
	} lbuf[MAX_BUF];
} d;
	static int
n_sbuf,
n_lbuf; /* no. values in s/l_bif */
	static
flsh_l() {
	int i;

	putc(LMASK + n_lbuf - 1, file);
	for (i = 0; i < n_lbuf; ++i) {
		putc(d.lbuf[i].x >> 8, file);
		putc(d.lbuf[i].x     , file);
		putc(d.lbuf[i].y >> 8, file);
		putc(d.lbuf[i].y     , file);
	}
	n_lbuf = 0;
}
	static
flsh_s() {
	putc(SMASK + n_sbuf - 1, file);
	fwrite(d.sbuf, 2, n_sbuf, file);
	n_sbuf = 0;
}
	static
bflsh() { /* flush both buffers */
	if (n_lbuf) flsh_l();
	else if (n_sbuf) flsh_s();
}
	extern long
lrnd();
	static char
inits[INITLEN] = { 0, 0x02, 0x70 },
s[] = "\0";
	static XYS
cursor = { 0, 0, s };
	static long
fxls,
fyls;
	static int
pendown,
oldpen;
	XYS *
Dc970(va_alist) va_dcl {
	va_list vap;
	int cmd, cnt;
	long xv, yv, pen;
	XYS *ret = &cursor;
	double r;
	long m, n, adx, ady, dx, dy;

	va_start(vap); cmd = va_arg(vap, int);
	switch (cmd) {
	case D_SCALE:
		if ((Dglobal.scale *= 4.) <= 0.) Dglobal.scale = 4.;
		goto scaleit;
	case D_INIT:
		fwrite(inits, 1, INITLEN, file);
		pendown = oldpen = 0;
		fxls = fyls = 0;
		Dglobal.scale *= 4.;
scaleit:
		if (Dglobal.reverse) { cursor.x = YPMAX; cursor.y = XPMAX; }
		else		{ cursor.y = YPMAX; cursor.x = XPMAX; }
		cursor.x /= Dglobal.scale;
		cursor.y /= Dglobal.scale;
		break;
	case D_DONE:
		bflsh();
		putc(PENUP, file);
		break;
	case D_MOVE:
		if (pendown) {
			bflsh();
			pendown = 0;
			putc(PENUP, file);
		}
		goto moveit;
	case D_LINE:
		if (!pendown) {
			bflsh();
			pendown++;
			putc(PENDOWN, file);
		}
moveit:
		xv = va_arg(vap, long);
		yv = va_arg(vap, long);
		if (Dglobal.reverse) {
			ady = xv * Dglobal.scale + .5;
			adx = XPMAX - (long)(yv * Dglobal.scale + .5);
		} else {
			ady = yv * Dglobal.scale + .5;
			adx = xv * Dglobal.scale + .5;
		}
		dx = adx - fxls;
		dy = ady - fyls;
		fxls = adx;
		fyls = ady;
		adx = abs(dx);
		ady = abs(dy);
		if (!adx && !ady) break; /* no motion */
		if (adx < 128 && ady < 128) {
				/* short deltas */
			if (n_lbuf) flsh_l();
			if (n_sbuf >= MAX_BUF) flsh_s();
			d.sbuf[n_sbuf].x = dx;
			d.sbuf[n_sbuf++].y = dy;
		} else {
				/* long deltas */
			if (n_sbuf) flsh_s();
			if (adx > 32767 || ady > 32767) {
				m = adx > ady ? adx : ady;
				cnt = m / 32767;
				if (!(cnt % 32767))
					--cnt;
				do {
					if (n_lbuf >= MAX_BUF) flsh_l();
					r = 1. / (cnt + 1);
					m = dx * r;
					n = dy * r;
					d.lbuf[n_lbuf].x = m;
					d.lbuf[n_lbuf++].y = n;
					dx -= m;
					dy -= n;
				} while (--cnt);
			}
			if (n_lbuf >= MAX_BUF) flsh_l();
			d.lbuf[n_lbuf].x = dx;
			d.lbuf[n_lbuf++].y = dy;
		}
		break;
	case D_PEN:
		pen = (va_arg(vap, long)) & 0x3; /* 4 pens */
		bflsh();
		if (pen != oldpen) {
			putc(PENUP, file);
			putc(0x02, file);
			putc(PENMASK + pen, file);
			fyls += (pen - oldpen) * PENOFF;
			oldpen = pen;
			pendown = 0;
		}
		break;
	}
	va_end(vap);
	return ret;
}
