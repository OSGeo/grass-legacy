#ifndef lint
static char *SCCSID = "@(#)Dingena.c	OEMG v.3.2";
#endif
/* General non-4010 interactive graphic terminal driver */
#include <varargs.h>
#include "plotter.h"
#define out stdout
/* device specific options:
	model no.:
	0	Hewlett-Packard 2647
	1	Hewlett-Packard 2393A
	2	Tektronic 4027 native mode
*/
static char s[2];		/* required structure for return */
static XYS cursor = {0, 0, s};	/* data.  "s" may be larger, if needed */
	static int
lastxy = 0,
xl,
yl,
enabled = 0;
static struct {
	char	*prelude, *postlude;	/* init and closure string */
	char	*LASTXY;		/* x-y stream terminator */
	char	*curon, *curoff;	/* cursor on/off */
	char	*erases;		/* clear screen */
	char	*bell;			/* ring bell */
	int	xpmax, ypmax;		/* max raster range */
} *modelp, model[] = {
		/* hp2647 */
	"\033*da\n\033*dc\n\033*df\n\033*dl\n",
	"\033*dd\n\033*de\n", 
	" Z\n",
	"\033*dk\n\033*s4^\n", "\033*dl\n",
	"\033*da\n",
	"\007",
	719, 359,
		/* hp2393A */
	"\033*dA\033*dC\033*dF\033*dL",
	"\033*dD\033*dE", 
	" Z",
	"\033*dK\033*s4^", "\033*dL",
	"\033*dA",
	"\007",
	511, 389,
		/* Tektronix 4027 */
	"\037wor 33\037gra 1,33\n",
	"\037wor 0\n",
	"\n",
	"\037ena 1\n", "",
	"\037era g\n",
	"\037str /\007/\n",
	639, 447
};

# define XPMAX modelp->xpmax
# define YPMAX modelp->ypmax
# define UNABLE if (lastxy) {fputs(modelp->LASTXY, out); lastxy = 0;}
	XYS *
Dingena(va_alist) va_dcl {
	va_list vap;
	int cmd, i, c, pen;
	long xv, yv;
	char curbuf[30];
	int x, y;
	XYS *ret = &cursor;

	va_start(vap); cmd = va_arg(vap, int);
	switch(cmd) {
	case D_SCALE:
		if (Dglobal.scale <= 0.)
			Dglobal.scale = 1.;
		goto scaleit;
	case D_INIT:
		lastxy = 0;
		modelp = model + Dglobal.model_no;
		/* clear gr. mem, gr on, al off, cursor off */
		fputs(modelp->prelude, out);
scaleit:
		if (Dglobal.reverse)	{ cursor.x = YPMAX; cursor.y = XPMAX; }
		else		{ cursor.y = YPMAX; cursor.x = XPMAX; }
		cursor.x = cursor.x / Dglobal.scale;
		cursor.y = cursor.y / Dglobal.scale;
		break;
	case D_DONE:
		UNABLE
		if (!Dglobal.quiet && out == stdout) {
			fflush(stdin);
			fputs(modelp->bell, out);
			getc(stdin);
		}
	case  D_PANIC:
		/* gr off, ascii on */
		fputs(modelp->postlude, out);
		enabled = 0;
		break;
	case D_DISABL:
		UNABLE
		fflush(out);
		break;
	case D_MOVE:
		UNABLE
		lastxy = 0;
	case D_LINE:
		if (!enabled && lastxy) {
			switch (Dglobal.model_no) {
			case 0:
			case 1:
				fprintf(out,"\033*pa %d,%d", xl, yl);
				break;
			case 2:
				fprintf(out,"\037vec %d,%d", xl, yl);
				break;
			default:
bummodel:
				fprintf(stderr, "invalid model number\n");
				exit(1);
			}
			enabled = 1;
		}
		xv = va_arg(vap, long);
		yv = va_arg(vap, long);
		if (Dglobal.reverse) {
			y = xv * Dglobal.scale + .5;
			x = XPMAX - (long)(yv * Dglobal.scale + .5);
		} else {
			y = yv * Dglobal.scale + .5;
			x = xv * Dglobal.scale + .5;
		}
		switch (Dglobal.model_no) {
		case 0:
		case 1:
			fprintf(out, "%s %d,%d", lastxy ? "" : "\033*pa", x, y);
			break;
		case 2:
			fprintf(out,"%s%d,%d", lastxy ? "," : "\037vec ", x, y);
			break;
		default:
			goto bummodel;
		}
		xl = x;
		yl = y;
		lastxy = 1;
		enabled = 1;
		break;
	case D_ERASE:
		UNABLE
		fputs(modelp->erases, out);
		fflush(out);
		break;
	case D_PEN:
		switch (Dglobal.model_no) {
		case 2:
			UNABLE
			pen = (va_arg(vap, long)) & 0x7;
			fprintf(out,"\037col C%d\n", pen);
			break;
		}
		break;
	case D_CURSOR:
		UNABLE
		/* turn on cursor and get value with keystroke */
		fflush(stdin);
		fputs(modelp->curon, out);
		fflush(out);
		fgets(curbuf, 25, stdin);
		fputs(modelp->curoff, out); /* turn off cursor */
		switch (Dglobal.model_no) {
		case 0:
		case 1:
			i = sscanf(curbuf,"+%d,+%d,%d", &cursor.x,
				&cursor.y, &c);
			break;
		case 2:
			i = sscanf(curbuf,"\037DAT 03,%d,%d,%d",
				&c,&cursor.x,&cursor.y);
			break;
		default:
			goto bummodel;
		}
		if (i != 3) {
			ret = (XYS *)0;
			break;
		}
		*cursor.s = c & 0x7f;
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
		;
	}
	va_end(vap);
	return ret;
}
