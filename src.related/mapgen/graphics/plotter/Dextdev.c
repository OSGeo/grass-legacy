#ifndef lint
static char *SCCSID = "@(#)Dextdev.c	USGS v.4.6";
#endif
/* General linkage to external non-interactive device
** by call to intermediate translation program
**
**	The target device translator is expected to be a true filter routine
**	so that the local 'reopening' of stdout will affect the output
**	of the translator.
**
**	The translators are expected to reside in the same path as
**	program plotter (defined in the environment).
**
**	The new graphic stream should be system independent.
**
**	-DRxsize,ysize - option to set size of [XY]PMAX dynamically.
**		principly employed by szoom (Sun zoom).
*/
# include <signal.h>
#ifdef SIGCLD
/* AT&T */
#define SYS_SIGCLD SIGCLD
#else
#ifdef SIGCHLD
/* Berkeley */
#define SYS_SIGCLD SIGCHLD
#endif
#endif
# include <varargs.h>
# include "plotter.h"
#define EXTDEV
#define PLOTTER
# include "graphics.h"
#define CTS_CM	200

static char s[] = "\0";		/* required structure for return */
static XYS cursor = {0, 0, s};	/* data.  "s" may be larger, if needed */

/* determine the size of the plotter and put the appropriate
** maximum range values into the following definitions.
*/
struct ext_list {
	char	*prog;		/* name of translation program [-args] */
	long	xsize, ysize;	/* size of plotter */
} model[] = {
	"extdebug", 15000, 10000,	/* simple listing routine */
	"c1077", 65535, 17525,		/* Calcomp 1077 */
	"dp8", 65535, 17270,		/* Houston DP8 */
	"apple", 5048, 4064,		/* Apple Laser Writer */
	"imogen", 5048, 4064,		/* Imogen Laser */
	"c5800", 65535, 22350,		/* Calcomp 5800 electrostatic */
	"sundump",4000,3000,		/* Simple version of SunCore */
	(char *)0,910,700,		/* Zoom version of SunCore */
};
# define XPMAX model[Dglobal.model_no].xsize
# define YPMAX model[Dglobal.model_no].ysize
	static
oldpen = -1;
	static FILE *
translat;
	static char
ssize[30],
name[80];
	static void
dead_child() {
	bomb(0,"external driver %s died or non-existant\n",
		model[Dglobal.model_no].prog);
}
	XYS *
Dextdev(va_alist) va_dcl {
	va_list vap;
	int cmd, i;
	char	*getenv(), *strrchr(), *ss;
	long	x, y, t, strtol();
	unsigned long pen;
	XYS *ret = &cursor;

	va_start(vap); cmd = va_arg(vap, int);
	switch(cmd) {
	case D_SCALE:
		if (Dglobal.scale <= 0.)
			Dglobal.scale = 1.;
		goto scaleit;
	case D_INIT:
		/* check veracity of model number */
		if (Dglobal.model_no < 0 || Dglobal.model_no >=
			(sizeof(model) / sizeof(struct ext_list)))
			bomb(0,"external model number %d invalid\n",
				Dglobal.model_no);
		/* check if device size changed by -DRx,y runline option */
		for (i = 1; i < Dglobal.dargc; ++i)
			switch (*(ss = Dglobal.dargv[i])) {
			case 'R':
				XPMAX = strtol(++ss, &ss, 0);
				YPMAX = strtol(++ss, &ss, 0);
				if (XPMAX <= 0 || YPMAX <= 0)
					bomb(0,"-DR%d,%d invalid\n",
						XPMAX,YPMAX);
				break;
			}
		if (model[Dglobal.model_no].prog) {
			strcpy(name, getenv(_GENVB));
			*(strrchr(name, '/') + 1) = '\0';
			strcat(name, model[Dglobal.model_no].prog);
			Dglobal.dargv[0] = name;
			sprintf(ssize,"-S%d,%d,%d",XPMAX,YPMAX,CTS_CM);
			Dglobal.dargv[Dglobal.dargc] = ssize;
			if (!(translat = wpopen(Dglobal.dargv)))
				bomb(1,"ext. driver %s failed to exec\n",name);
			(void)signal(SYS_SIGCLD, dead_child);
		} else
			translat = stdout;
		putc(_BOP, translat);
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
		if (!translat) break;
		putc(_EOP, translat);
		if (translat != stdout) {
			(void)signal(SYS_SIGCLD, SIG_DFL);
			fflush(translat);
			wpclose(translat);
		}
		break;
	case D_MOVE:
	case D_LINE:
		x = (va_arg(vap, long)) * Dglobal.scale;
		y = (va_arg(vap, long)) * Dglobal.scale;
		if (Dglobal.reverse) {
			t = x;
			x = y;
			y = YPMAX - t;
		}
		putc(cmd == D_MOVE ? _MOVE : _DRAW, translat);
		putc(x >> 8, translat);
		putc(x, translat);
		putc(y >> 8, translat);
		putc(y, translat);
		break;
	case D_PEN:
		pen = va_arg(vap, unsigned long);
		if (pen != oldpen) {
			oldpen = pen;
			if (pen > 0xff) {
				putc(_PENL, translat);
				putc(pen >> 16, translat);
				putc(pen >> 8, translat);
			} else
				putc(_PEN, translat);
			putc(pen, translat);
		}
		break;
	case D_STRING:
		ss = va_arg(vap, char *);
		putc(_SPCL, translat);
		while (*ss) putc(*ss++, translat);
		putc('\0', translat);
		break;
	}
	va_end(vap);
	return ret;
}
