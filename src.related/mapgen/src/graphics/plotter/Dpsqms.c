# ifndef lint
static char *SCCSID = "@(#)Dpstscr.c	OEMG v.3.1";
# endif
/* pstscr - driver for PostScript output */
# define PLOTTER

# include <varargs.h>
# include "plotter.h"

# define XPMAX	9900 /* 11 x 8.5 inches scaled as a */
# define YPMAX	8000 /* 200 cnts/cm. engine */

static char s[] = "\0";		/* required structure for return */
static XYS cursor = {0, 0, s};	/* data.  "s" may be larger, if needed */

	static int
troff = 0,
noshow = 0,
oldpen = 0,
pen,
pens[8] = {1,5,10,15,20,30,40,50},
pendown;

	float
qmsgray[4] = {0, .25, .5, .75 };
/* initialization strings and PostScript abbreviations */
/* \004 tag set to indicate beginning of PostScript data */

	static char
*initstr = "\
%START\n\
/S { stroke } bind def\n\
/L { rlineto } bind def\n\
/U { moveto } bind def\n\
1 setlinecap\n\
1 setlinejoin\n\
gsave\n",

*init2 = "\
0 0 translate\n\
-90 rotate\n\
.0785987734 .0785987734 scale\n\
1 setlinewidth\n\
",

*landstr = "\
",

*portstr = "\
";

	static long
x, y, xl, yl, dx, dy;

	extern long
strtol();

/* ######################### Start of plot routine ###################################*/
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
		if (Dglobal.scale <= 0.)
			Dglobal.scale = 1.;
		goto scaleit;
	case D_INIT:
		for (i = x = y = 0; i < Dglobal.dargc ; ++i)
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
				fprintf(stderr,"unknown arg:%s\n",
					Dglobal.dargv[i]);
				ret = (XYS *)0;
			}
		if (!ret) break;
		pendown = 0;
		if (!troff)
			fputs(initstr, stdout);
		fputs(init2, stdout);
		fputs(Dglobal.reverse?portstr:landstr, stdout);
		if (x || y)
			fprintf(stdout, "%d %d translate\n",x,y);
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
		if (pendown)
			fputs("S\n", stdout);
		if (!troff)
			fputs("grestore\n", stdout);
		if (!noshow) {
			fputs("showpage\n", stdout);
			fputs("\004\n", stdout);
		}
		break;
	case D_MOVE:
		if (pendown) {
			if (!dx && !dy) /* ensure at least 2 pts. */
				fputs("0 0 L ", stdout);
			fputs("S\n", stdout);
			dx = 1;
			pendown = 0;
		}
	case D_LINE:
		x = (va_arg(vap, long) - XPMAX) * Dglobal.scale + .5;
		y = (va_arg(vap, long) + 250) * Dglobal.scale + .5;
		if (pendown) {
			if ((dx = x - xl) | (dy = y - yl))
				fprintf(stdout,"%d %d L\n", dx, dy);
		} else {
			fprintf(stdout,"%d %d U\n", x, y);
			dy = dx = 0;
			pendown = 1;
		}
		xl = x;
		yl = y;
		break;
	case D_PEN:
		lpen = va_arg(vap, long);
		pen = lpen & 0x7; /* 8 pen widths */
		if (pen != oldpen) {
			if (pendown) {
				if (!dx && !dy) /* ensure at least 2 pts. */
					fputs("0 0 L ", stdout);
				fputs("S\n", stdout);
			}
			fprintf(stdout,"%d setlinewidth %.3f setgray\n",
				pens[pen],
				qmsgray[(lpen>>3)&3]);
			dx = 1;
			pendown = 0;
			oldpen = pen;
		}
		break;
	}
	va_end(vap);
	return ret;
}
