#ifndef lint
static char *SCCSID = "@(#)Dgfx.c	OEMG v.3.1";
#endif
/* EGA driver for Microport System V/386 gfx driver */

#include <varargs.h>
#include "plotter.h"
	static int
xlast, ylast,	/* last pen position */
pen = 1,	/* pen color */
enabled = 0;	/* graphics enabled */

/* gfx ega code *******************************************************/
# include <sys/types.h>
# include <fcntl.h>
# include <termio.h>
# include <sys/kd_info.h>
# include "gfx.h"

#define ROWS 350
#define BCOLS 80

	static int
egafd;
	static char
*ega_buffer,
*ega_end,
*ega_outp;

#define	ega_out(v,t)	{ *(t *) ega_outp = (t) (v); ega_outp += sizeof (t); }
#define	char_out(v)	{ *ega_outp++ = (v); }
#define	int_out(v)	{ ega_out(v, unsigned short); }

ega_flush () {
	int cnt = ega_outp - ega_buffer;
	write (egafd, ega_buffer, cnt);
	ega_outp = ega_buffer;
}
#define	CHECK(x) { if ((ega_outp + x) > ega_end) ega_flush (); }
#define	clrega(to, cnt) { \
	CHECK (5); char_out (gfx_clearram); int_out (to); int_out (cnt); }
#define	fillega(data, to, inc, cnt) { \
	CHECK (8); char_out (gfx_fill); char_out (data); \
	int_out (to); int_out (inc); int_out (cnt); }
#define set_datarot(d) { CHECK (2); char_out (gfx_datarot); char_out (d); }
#define	set_readmask(d) { CHECK (2); char_out (gfx_readmask); char_out (d); }
#define	set_writemode(d) { CHECK (2); char_out (gfx_writemode); char_out (d); }
#define	set_bitmask(m) { CHECK (2); char_out (gfx_bitmask); char_out (m); }
#define set_mapmask(m) { CHECK (2); char_out (gfx_mapmask); char_out (m); }
#define set_color(d) { CHECK (2); char_out (gfx_color); char_out (d); }
#define	set_colorenab(d) { CHECK (2); char_out (gfx_colorenab); char_out (d); }
#define	set_xbyte0(ram) { CHECK (3); char_out (gfx_xbyte0); int_out (ram); }
#define	set_xbyte2(color, ram) { \
	CHECK (4); char_out (gfx_xbyte2); char_out (color); int_out (ram); }
#define ENDVECT ioctl(egafd, KDALPHAMODE, 0)
#define STARTVECT ioctl(egafd, KDSETMODE, 0x10)
	static
gfx_ega_init (name) char *name; {
	long ega_buffsize;
	
	if ((egafd = open(name, O_WRONLY)) < 0) return 0;
	if (! ega_buffer) { /* set ega specific */
		if (ioctl (egafd, KDGETBUFFSIZE, &ega_buffsize) < 0) return 0;
		if (!(ega_buffer = (char *) malloc ((unsigned) ega_buffsize)))
			return 0;
		ega_end = ega_buffer + ega_buffsize;
	}
	ega_outp = ega_buffer;
	ioctl(egafd, KDSETMODE, 0x10);
	set_mapmask(0xFF);	/* enable all planes	*/
	set_readmask(0xff);
	set_color(0);		/* background color	*/
	ega_flush();
	return(1);
}
gfx_ega_clear (color) int color; {
#ifdef DONT
	/* example code which fails on some situations */
	set_mapmask(-1);
	set_bitmask(-1);
	set_writemode(0);
	set_color(color);
	set_colorenab(0xff);
	clrega ((short)0, (short)(ROWS * BCOLS));
#else
	/* perhaps slower, but it works so far */
	set_writemode(2);
	set_datarot(0);
	set_bitmask(0xff);
	fillega(color, 0, 1, ROWS*BCOLS);
#endif
	ega_flush();
}

	/* graphics controller data rotate port (3) values */
# define UNMOD 0x00	/* source data unmodified */
# define ANDED 0x08	/* source data ANDed */
# define ORED 0x10	/* source data ORed */
# define XORED 0x18	/* source data XORed */
	static int
rotate = UNMOD;
	static
color,
type;
	static
vertline(x, y1, y2) {
	static int t;

	if (y1 > y2) { t = y1; y1 = y2; y2 = t; }
	set_bitmask(0x80 >> ( x & 7 ));
	fillega(color, (x >> 3) + 80 * y1, 80, y2 - y1 + 1);
}
	static
horzline(x1, x2, y) {
	static int i1, i2, lmask, rmask, t, base;

	if (x1 > x2) { i1 = x1; x1 = x2; x2 = i1; }
	base = ( x1 >> 3 ) + 80 * y;
	i1 = x1 >> 3; lmask = 0xff >> (x1 & 7);
	i2 = x2 >> 3; rmask = 0xff80 >> (x2 & 7);
	if (!(t = i2 - i1)) {
		set_bitmask(lmask & rmask);
		set_xbyte2(color, base);
	} else {
		set_bitmask(lmask);
		set_xbyte2(color, base++);
		if (--t) {
			set_bitmask(0xff);
			fillega(color, base, 1, t);
			base += t;
		}
		set_bitmask(rmask);
		set_xbyte2(color, base);
	}
}
	static
dda(x, y, dx, dy) {
	static e, ddy, ddxy, ix, iy, is, ie;

	if (dx < 0) { dx = -dx; ix = -1; } else ix = 1;
	if (dy < 0) { dy = -dy; iy = -1; } else iy = 1;
	is = x; ie = -1;
	for (ddxy = ( e = (ddy = dy + dy) - dx) - dx; dx-- >= 0 ; x += ix ) {
		ie = x;
		if (e > 0) {
			if (type) vertline(y, is, ie);
			else horzline(is, ie, y);
			y += iy;
			e += ddxy;
			is = x+ix;
			ie = -1;
		} else	
			e += ddy;
	}
	if (ie >= 0)
		if (type) vertline(y, is, ie);
		else horzline(is, ie, y);
}
	static
egavect(x1, y1, x2, y2) unsigned short x1, y1, x2, y2; {
	static short dx, dy;

	color = pen;
	set_writemode(2);
	set_datarot(rotate);
	if (!(dx = x2 - x1))
		vertline(x1, y1, y2);
	else if (!(dy = y2 - y1))
		horzline(x1, x2, y1);
	else if (type = (abs(dx) < abs(dy)))
		dda(y1, x1, dy, dx);
	else
		dda(x1, y1, dx, dy);
	ega_flush();
}
	/* cursor position */
	static 
xclast = 0,
yclast = 349;
# define CUR_SIZE 10
	static int
cur_bump = 1;
	static struct termio
old, new;
	static
getcursor(x, y) int *x, *y; {
	static char s[4];
	int yold, xold, x1, y1, x2, y2, opr;

	fflush(stdout);
	/* set tty to no echo so we can get cursor stream */
	if (ioctl(0, TCGETA, &old) < 0) {
		fprintf(stderr, "setstrm error\n");
		exit(1);
	}
	new = old;
	new.c_lflag = ISIG;
	new.c_cc[VTIME] = 1;
	new.c_cc[VMIN] = 3;
	ioctl(0, TCSETAW, &new);
	color = 0xf;
	set_writemode(2);
	set_datarot(XORED);
	for (opr = 3; opr != 1 ; ) {
		if ((y1 = yclast - CUR_SIZE) < 0) y1 = 0;
		if ((y2 = yclast + CUR_SIZE) > 349) y2 = 349;
		if ((x1 = xclast - CUR_SIZE) < 0) x1 = 0;
		if ((x2 = xclast + CUR_SIZE) > 639) x2 = 639;
		if (opr == 3) {
			vertline(xold = xclast, y1, y2); /* draw it */
			horzline(x1, x2, yold = yclast);
			ega_flush();
		}
		opr = read(0, s, 3);
		if (opr == 3) {
			if (s[0] == '\033') {
				if (s[1] == 'O')
					switch ( s[2] ) {
					case 'T': cur_bump = 1; break; /*F5*/
					case 'U': cur_bump = 2; break; /*F6*/
					case 'V': cur_bump = 5; break; /*F7*/
					case 'W': cur_bump = 10; break; /*F8*/
					case 'X': cur_bump = 20; break; /*F9*/
					default: goto opr0;
					}
				else if (s[1] == '[') {
					switch ( s[2] ) {
					case 'A': yclast -= cur_bump; break;
					case 'B': yclast += cur_bump; break;
					case 'D': xclast -= cur_bump; break;
					case 'C': xclast += cur_bump; break;
					case 'V': xclast += cur_bump;
						yclast -= cur_bump; break;
					case 'U': xclast += cur_bump;
						yclast += cur_bump; break;
					case 'H': xclast -= cur_bump;
						yclast -= cur_bump; break;
					case 'Y': xclast -= cur_bump;
						yclast += cur_bump; break;
					default: goto opr0;
					}
					if (xclast < 0) xclast = 0;
					else if (xclast > 639) xclast = 639;
					if (yclast < 0) yclast = 0;
					else if (yclast > 349) yclast = 349;
				} else
					goto opr0;
			} else
opr0:
				opr = 0;
		}
		if (opr) {
			vertline(xold, y1, y2); /* "erase" it */
			horzline(x1, x2, yold);
			ega_flush();
		}
	}
	/* reset input port */
	ioctl(0, TCSETAW, &old);
	fflush(stdin);  /* ???? */
	*x = xclast;
	*y = yclast;
	return (*s);
}
/* end ega code ***************************************************/

static char s[] = "\0";		/* required structure for return */
static XYS cursor = {0, 0, s};	/* data.  "s" may be larger, if needed */

	/* basic screen ranges (clone 4010 values ) */
# define XPMAX	1023
# define YPMAX	779
	/* 639 / 1023 */
# define TO_X_EGA .62463343108504398826
	/* 349 / 779 */
# define TO_Y_EGA .44801026957637997432
	static double
xscale, yscale;
	static char
color_map[15] = { 7, 6, 5, 4, 3, 2, 1, 15, 14, 13, 12, 11, 10, 9, 8 };
	XYS *
Dgfx(va_alist) va_dcl {
	va_list vap;
	int cmd, x, y;
	long xv, yv;
	XYS *ret = &cursor;

	va_start(vap); cmd = va_arg(vap, int);
	switch(cmd) {
	case D_SCALE:
		if (Dglobal.scale <= 0.)
			Dglobal.scale = 1.;
		goto scale_it;
	case D_INIT:
		{ char *name, *getenv();
			if (!(name = getenv("EGADEV")) ||
				! gfx_ega_init(name))
				break;
		}
		enabled = 1;
scale_it:
		xscale = Dglobal.scale * TO_X_EGA;
		yscale = Dglobal.scale * TO_Y_EGA;
		if (Dglobal.reverse) { cursor.x = YPMAX; cursor.y = XPMAX; }
		else		{ cursor.y = YPMAX; cursor.x = XPMAX; }
		cursor.x = cursor.x / Dglobal.scale + .5;
		cursor.y = cursor.y / Dglobal.scale + .5;
		break;
	case D_DONE:
		fflush(stdin);
		if (!Dglobal.quiet) {
			putc('\007', stdout);
			fflush(stdout);
			getc(stdin);
		}
	case D_PANIC:
		if (enabled) {
			ENDVECT;
			enabled = 0;
		}
		close(egafd);
		break;
	case D_DISABL:
		if (enabled) {
			ENDVECT;
			enabled = 0;
		}
		break;
	case D_MOVE:
	case D_LINE:
		xv = va_arg(vap, long);
		yv = va_arg(vap, long);
		if (Dglobal.reverse) {
			y = xv * xscale + .5;
			x = XPMAX - (long)(yv * yscale + .5);
		} else {
			y = yv * yscale + .5;
			x = xv * xscale + .5;
		}
		if (cmd == D_LINE) {
			if (!enabled) {
				STARTVECT;
				enabled = 1;
			}
			egavect(xlast, 349 - ylast, x, 349 - y);
		}
		xlast = x;
		ylast = y;
		break;
	case D_ERASE:
		if (!enabled) {
			STARTVECT;
			enabled = 1;
		}
		gfx_ega_clear(0);
		break;
	case D_PEN:
		pen = color_map[(va_arg(vap, long)) % 15];
		break;
	case D_CURSOR:
		if (!enabled) {
			STARTVECT;
			enabled = 1;
		}
		cursor.s[0] = getcursor(&cursor.x, &cursor.y);
		cursor.y = 349 - cursor.y;
		if (Dglobal.reverse) {
			static long temp;
			temp = cursor.y;
			cursor.y = XPMAX - cursor.x;
			cursor.x = temp;
		}
		cursor.x = cursor.x / xscale + .5;
		cursor.y = cursor.y / yscale + .5;
		break;
	}
	va_end(vap);
	return(ret);
}
