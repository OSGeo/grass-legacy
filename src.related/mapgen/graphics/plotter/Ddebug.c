#ifndef lint
static char *SCCSID = "@(#)Ddebug.c	USGS v.4.3";
#endif
/* Debugging driver */
#include "plotter.h"
#include <varargs.h>

static char s[2] = "\0";	/* required structure for return */
static XYS cursor = {0, 0, s};	/* data.  "s" may be larger */

	/* basic range */
# define XPMAX	3000
# define YPMAX	2000
static long old_pen = -1;
	XYS *
Ddebug(va_alist) va_dcl {
	va_list vap;
	int cmd, i;
	long x, y, pen;
	XYS *ret = &cursor;

	va_start(vap); cmd = va_arg(vap, int);
	switch(cmd) {
	case D_SCALE:	/* set scaling */
		printf("D_SCALE: %g\n", Dglobal.scale);
		goto rescale;
	case D_INIT:	/* initialization, print Dglobal values */
		printf("D_INIT: scale: %g, model_no: %d\n",
			Dglobal.scale, Dglobal.model_no);
		printf("\treverse: %s, quiet: %s\n",
			Dglobal.reverse ? "ON" : "OFF",
			Dglobal.quiet ? "ON" : "OFF");
		printf("\t%d -Dargs\n", Dglobal.dargc-1);
		for (i = 1; i < Dglobal.dargc; ++i)
			printf("\t\t%s\n",Dglobal.dargv[i]);
			/* return maximum size of device */
rescale:
		cursor.x = XPMAX / Dglobal.scale;
		cursor.y = YPMAX / Dglobal.scale;
		break;
	case D_DONE:	/* normal completion */
		printf("D_DONE, hit return when done: ");
		if ( ! Dglobal.quiet ) {
			putchar('\007');
			fflush(stdin);
			(void)getchar();
		}
		break;
	case D_PANIC:	/* completion due to signal trap */
		printf("D_PANIC\n");
		break;
	case D_DISABL:	/* disable graphics mode (some terminals) */
		printf("D_DISABL\n");
		break;
	case D_MOVE:	/* move, pen-up */
		printf("D_MOVE");		
		goto printxy;
	case D_LINE:	/* move, pen-down (draw a line) */
		printf("D_LINE");
printxy:
		if (Dglobal.reverse) {
			y = va_arg(vap, long) * Dglobal.scale + 0.5;
			x = XPMAX - va_arg(vap, long) * Dglobal.scale + 0.5;
		} else {
			x = va_arg(vap, long) * Dglobal.scale + 0.5;
			y = va_arg(vap, long) * Dglobal.scale + 0.5;
		}
		printf(" x/y: %6ld %6ld\n", x, y);
		break;
	case D_ERASE:	/* erase (clear screen), NO-OP on hard copy */
		printf("D_ERASE\n");		
		break;
	case D_PEN:	/* select plotter's mechanical pen */
		pen = va_arg(vap, long);
		printf("D_PEN: %ld (replacing: %ld)\n", pen, old_pen);
		old_pen = pen;
		break;
	case D_STRING:  /* device dependent string */
		printf("D_STRING: <%s>\n", va_arg(vap, char *));
		break;
	case D_CURSOR:	/* get cursor location and key */
		for(;;) {
			printf("D_CURSOR: enter x y c\n");
			fflush(stdin); /* no type-ahead */
			if (scanf("%ld %ld %1s",&cursor.x,&cursor.y,
			   s) == 3) break;
			printf("?\n");
		}
		if (Dglobal.reverse) {
			cursor.y = cursor.x / Dglobal.scale + .5;
			cursor.x = cursor.y / Dglobal.scale + .5;
		} else {
			cursor.x = cursor.x / Dglobal.scale + .5;
			cursor.y = cursor.y / Dglobal.scale + .5;
		}
		break;
	default:	/* should never occur, see system manager */
		printf("unknown operation\n");
		break;
	}
	va_end(vap); /* cleanup varargs */
	return(ret);
}
