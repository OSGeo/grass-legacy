# ifndef lint
static char *SCCSID = "@(#)Dpsqms.c	USGS v.4.7";
# endif
/* psqms - driver for PostScript output */
# define PLOTTER

# include <varargs.h>
# include "plotter.h"

# define XPMAX	10849 /* 11 x 8.5 inches scaled as a */
# define YPMAX	8636 /* 400 cnts/cm. engine */

static char s[] = "\0";		/* required structure for return */
static XYS cursor = {0, 0, s};	/* data.  "s" may be larger, if needed */
#define MAX_DXY 50
	static XY
dxy[MAX_DXY];
	static int
nline = 0,
ndxy = 0,
troff = 0,
noshow = 0,
oldpen = 0,
pen,
pens[8] = {1,5,10,15,20,30,40,50},
pendown;
	float
gray[4] = {0, .25, .5, .75 };
/* initialization strings and PostScript abbreviations */
/* \004 tag set to indicate beginning of PostScript data */
	static char
*initstr = "\
%!PS\n\
/L { {rlineto} repeat currentpoint stroke moveto} bind def\n\
/U { moveto } bind def\n\
1 setlinecap\n\
1 setlinejoin\n\
gsave\n",
*init2 = "\
.14226378 .14226378 scale\n\
1 setlinewidth\n\
",
*landstr = "\
0 5588 translate\n\
-90 rotate\n\
",
*portstr = "\
";
	static void
dump() {
	int n;

	if (!ndxy) {
		if (pendown > 1)	return;
		dxy[0].x = dxy[0].y = 0;
		++ndxy;
	}
	n = ndxy;
	while (n--) /* print out in reverse order */
		if ((nline += printf("%d %d ",dxy[n].x,dxy[n].y)) > 70 && n) {
			(void)putchar('\n');
			nline = 0;
		}
	(void)printf("%d L\n",ndxy);
	nline = ndxy = 0;
	++pendown;
}
	static long
x, y, xl, yl, dx, dy;
	extern long
strtol();
	XYS *
Dpsqms(va_alist) va_dcl {
	va_list vap;
	int cmd, i;
	long lpen;
	XYS *ret = &cursor;
	char *ss;

	va_start(vap); cmd = va_arg(vap, int);
	switch(cmd) {
	case D_SCALE:
		if (Dglobal.scale <= 0.)	Dglobal.scale = 1.;
		goto scaleit;
	case D_INIT:
		Dglobal.scale = Dglobal.scale / 2.024;
		for (x = y = 0, i = 1; i < Dglobal.dargc ; ++i)
			switch (*Dglobal.dargv[i]) {
			case 't':	/* troff mode */
				troff = 1;
				Dglobal.reverse = 1;
			case 'x':	/* suppress showpage */
				noshow = 1;
				break;
			case 'o':	/* offset -o<x>,<y> */
				ss = Dglobal.dargv[i];
				x = strtol(++ss, &ss, 0);
				y = strtol(++ss, &ss, 0);
				break;
			default:
				(void)fprintf(stderr,"unknown arg:%s\n",
					Dglobal.dargv[i]);
				ret = (XYS *)0;
			}
		if (!ret) break;
		pendown = 0;
		if (!troff)	puts(initstr);
		puts(init2);
		puts(Dglobal.reverse?portstr:landstr);
		if (x || y)	(void)printf("%d %d translate\n",x,y);
scaleit:
		if (Dglobal.reverse) {
			cursor.y = (troff ? 10000 : XPMAX) / Dglobal.scale;
			cursor.x = (troff ? 10000 : YPMAX) / Dglobal.scale;
		} else {
			cursor.x = XPMAX / Dglobal.scale;
			cursor.y = YPMAX / Dglobal.scale;
		}
		break;
	case D_DONE:
	case D_PANIC:
		if (pendown)	dump();
		if (!troff)	puts("grestore\n");
		if (!noshow)	puts("showpage\n\004\n");
		break;
	case D_MOVE:
		if (pendown) {
			dump();
			pendown = 0;
		}
	case D_LINE:
		x = (va_arg(vap, long)) * Dglobal.scale + .5;
		y = (va_arg(vap, long)) * Dglobal.scale + .5;
		if (pendown) {
			if ((dx = x - xl) | (dy = y - yl)) {
				dxy[ndxy].x = dx;
				dxy[ndxy].y = dy;
				if (++ndxy >= MAX_DXY) dump();
			}
		} else {
			nline = printf("%d %d U ", x, y);
			pendown = 1;
		}
		xl = x;
		yl = y;
		break;
	case D_PEN:
		lpen = va_arg(vap, long);
		pen = lpen & 0x7; /* 8 pen widths */
		if (pen != oldpen) {
			if (pendown)	dump();
			(void)printf("%d setlinewidth %.3f setgray\n",
				pens[pen],
				gray[(lpen>>3)&3]);
			pendown = 0;
			oldpen = pen;
		}
		break;
	}
	va_end(vap);
	return ret;
}
