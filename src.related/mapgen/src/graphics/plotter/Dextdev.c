static char *SCCSID = "@(#)Dextdev.c	OEMG v.3.2";
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
**	The new meta-graphic stream should be system independent.
**
**	-DRxsize,ysize - option to set size of [XY]PMAX dynamically.
**		principly employed by szoom (Sun zoom).
*/
# include <varargs.h>
# include "plotter.h"
#define EXTDEV
#define PLOTTER
# include <graphics.h>

static char s[] = "\0";		/* required structure for return */
static XYS cursor = {0, 0, s};	/* data.  "s" may be larger, if needed */

/* determine the size of the plotter and put the appropriate
** maximum range values into the following definitions.
*/
struct {
	char	*prog;		/* name of translation program [-args] */
	long	xsize, ysize;	/* size of plotter */
} model[] = {
	"extdebug", 10000, 10000,	/* simple listing routine */
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
name[80];
	XYS *
Dextdev(va_alist) va_dcl {
	va_list vap;
	int cmd, i;
	char	*getenv(), *strrchr(), *ss;
	long	x, y, t, pen, strtol();
	XYS *ret = &cursor;

	va_start(vap); cmd = va_arg(vap, int);
	switch(cmd) {
	case D_SCALE:
		if (Dglobal.scale <= 0.)
			Dglobal.scale = 1.;
		goto scaleit;
	case D_INIT:
		for (i = 0; i < Dglobal.dargc; ++i)
			switch (*(ss = Dglobal.dargv[i])) {
			case 'R':
				XPMAX = strtol(++ss, &ss, 0);
				YPMAX = strtol(++ss, &ss, 0);
				break;
			}
		if (model[Dglobal.model_no].prog) {
			strcpy(name, getenv(_GENVB));
			*(strrchr(name, '/') + 1) = '\0';
			strcat(name, model[Dglobal.model_no].prog);
			if (!(translat = popen(name, "w"))) {
				fprintf(stderr,"can't exec %s\n",name);
				exit(1);
			}
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
		putc(_EOP, translat);
	case D_PANIC:
		if (translat != stdout)
			pclose(translat);
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
		pen = (va_arg(vap, long));
		if (pen != oldpen) {
			oldpen = pen;
			putc(_PEN, translat);
			putc(pen >> 8, translat);
			putc(pen, translat);
		}
	}
	va_end(vap);
	return ret;
}
