#ifndef lint
static char *SCCSID = "@(#)Ddebug.c	OEMG v.3.1";
#endif
/* Debugging driver */
#include "plotter.h"
#include <varargs.h>

static char s[] = "\0";		/* required structure for return */
static XYS cursor = {0, 0, s};	/* data.  "s" may be larger, if needed */

	/* basic screen ranges (clone 4010 values ) */
# define XPMAX	1023
# define YPMAX	779
	XYS *
Ddebug(va_alist) va_dcl {
	va_list vap;
	int cmd, i;
	long xv, yv, pen;
	XYS *ret = &cursor;

	va_start(vap); cmd = va_arg(vap, int);
	switch(cmd) {
	case D_SCALE:
		printf("D_SCALE: %g\n", Dglobal.scale);
		break;
	case D_INIT:
		printf("D_INIT: scale: %g, model_no %d\n",
			Dglobal.scale, Dglobal.model_no);
		printf("\treverse: %s, quiet: %s\n",
			Dglobal.reverse ? "ON" : "OFF",
			Dglobal.quiet ? "ON" : "OFF");
		printf("\t%d -Dargs\n", Dglobal.dargc);
		for (i = 0; i < Dglobal.dargc; ++i)
			printf("\t\t%s\n",Dglobal.dargv[i]);
		cursor.x = XPMAX;
		cursor.y = YPMAX;
		break;
	case D_DONE:
		printf("D_DONE\n");
		break;
	case D_PANIC:
		printf("D_PANIC\n");
		break;
	case D_DISABL:
		printf("D_DISABL\n");
		break;
	case D_MOVE:
		printf("D_MOVE");
		goto printxy;
	case D_LINE:
		printf("D_LINE");
printxy:
		xv = va_arg(vap, long);
		yv = va_arg(vap, long);
		printf(" x/y: %6ld %6ld\n",xv, yv);
		break;
	case D_ERASE:
		printf("D_ERASE\n");
		break;
	case D_PEN:
		pen = va_arg(vap, long);
		printf("D_PEN: %ld\n", pen);
		break;
	case D_CURSOR:
		break;
	default:
		printf("unknown operation\n");
		break;
	}
	va_end(vap);
	return(ret);
}
