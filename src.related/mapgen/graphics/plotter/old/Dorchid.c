#include <varargs.h>
#include "plotter.h"

static char s[200] = "\0";		/* required structure for return */
static XYS cursor = {0, 0, s};	/* data.  "s" may be larger, if needed */

	/* basic screen ranges */
# define XPMAX	799
# define YPMAX	599
	static int
red[] = {	0,	255,	255,	0,	222,	0,	255,	255,	255 },
green[] = {	0,	255,	0,	255,	43,	0,	255,	127,	0 },
blue[] = {	0,	255,	127,	0,	242,	255,	0,	0,	0 };
#define TEMP "/tmp/orchtmp"
	static int
enable = 1;


	static void
chkenable() {
	if (!enable) {
		Graph_Set();
/*DEBUG   fprintf (stderr, "restoring panel\n"); */
		Panel_restore(TEMP);
		Panel_delete(TEMP);
		enable = 1;
	}
}
	XYS *
Dorchid(va_alist) va_dcl {
	va_list vap;
	int i, pen, x, y, cmd, xv, yv;
	static int xvl, yvl;
	XYS *ret = &cursor;

	va_start(vap); cmd = va_arg(vap, int);
/*DEBUG fprintf (stderr, "Dorchid %d\n",cmd);*/
	switch(cmd) {
	case D_SCALE:
		if (Dglobal.scale <= 0.)
			Dglobal.scale = 1.;
/*DEBUG fprintf (stderr, "D_SCALE %d,%d\n",XPMAX,YPMAX);*/
		{ cursor.y = YPMAX; cursor.x = XPMAX; }
/*DEBUG fprintf (stderr, "in scaleit x%ld y%ld\n",cursor.x,cursor.y);*/
		cursor.x = cursor.x / Dglobal.scale +1;
		cursor.y = cursor.y / Dglobal.scale +1;
/*DEBUG fprintf (stderr, "in scaleit x%ld y%ld\n",cursor.x,cursor.y);*/
		break;
	case D_INIT:
/*DEBUG fprintf (stderr, "init called\n");*/
		Graph_Set();
		xvl = yvl = 0;
		{ cursor.y = YPMAX; cursor.x = XPMAX; }
/*DEBUG fprintf (stderr, "in init x%ld y%ld\n",cursor.x,cursor.y);*/
		cursor.x = cursor.x / Dglobal.scale;
		cursor.y = cursor.y / Dglobal.scale;
/*DEBUG fprintf (stderr, "in init x%ld y%ld\n",cursor.x,cursor.y);*/
		for (i= 0; i < 8; i++)
			reset_color(i, red[i], green[i], blue[i]);
		break;
	case D_DONE:
		putchar('\007');
/*DEBUG fprintf (stderr, "in done\n");*/
		getchar();
	case D_PANIC:
		Graph_Close();
/*DEBUG fprintf (stderr, "in panic\n");*/
		Panel_delete(TEMP);
		break;
	case D_DISABL:
		Panel_save(TEMP,0,599,0,799);
/*DEBUG fprintf (stderr, "in disable\n");*/
		Graph_Close();
		enable = 0;
		break;
	case D_MOVE:
		chkenable();
		xvl = va_arg(vap, long) * Dglobal.scale;
		yvl = va_arg(vap, long) * Dglobal.scale;
/*DEBUG   fprintf (stderr, "in move (%d,%d)\n", xvl, yvl); */
		break;
	case D_LINE:
		chkenable();
		xv = va_arg(vap, long) * Dglobal.scale;
		yv = va_arg(vap, long) * Dglobal.scale;
/*DEBUG   fprintf (stderr, "in line (%d,%d) (%d,%d) (%d,%d)\n %lf\n", XPMAX, YPMAX, xvl, yvl, xv, yv, Dglobal.scale); */
		draw_line(xvl, YPMAX-yvl, xv, YPMAX-yv);
		xvl = xv;
		yvl = yv;
		break;
	case D_ERASE:
		chkenable();
/*DEBUG fprintf (stderr, "in erase \n", xvl, yvl);*/
		Erase();
		break;
	case D_PEN:
		chkenable();
		pen = va_arg(vap, long);
/*DEBUG*/ fprintf (stderr, "in pen %d \n", pen);
		while (pen > 8) pen -= 8;
/*		pen = (pen & 7) + 1;*/
/*DEBUG*/ fprintf (stderr, "in pen %d \n", pen);
		color(pen);
		break;
	case D_CURSOR:
		chkenable();
		cursor.x = xvl;
		cursor.y = YPMAX- yvl;
/*DEBUG fprintf (stderr, "in cursor (%d, %d)\n", cursor.x, cursor.y);*/
		Get_location_with_pointer(&cursor.x, &cursor.y, &i);
/*DEBUG fprintf (stderr, "AFTER cursor (%d, %d) %d\n", cursor.x, cursor.y, i);*/
		cursor.y = YPMAX - cursor.y;
		putchar('\007');
		gets(s);
/*DEBUG fprintf (stderr, "AFTER upside cursor (%d, %d) %d\n", cursor.x, cursor.y, i);*/
		if (Dglobal.reverse) {
			static long temp;
			temp = cursor.y;
			cursor.y = XPMAX - cursor.x;
			cursor.x = temp;
		}
/*DEBUG fprintf (stderr, "AFTER reverse cursor (%d, %d) %d\n", cursor.x, cursor.y, i);*/
		cursor.x = cursor.x / Dglobal.scale + .5;
		cursor.y = cursor.y / Dglobal.scale + .5;
/*DEBUG fprintf (stderr, "Dglobal.scale = %lf\n", (double) Dglobal.scale)*/;
/*DEBUG fprintf (stderr, "AFTER global cursor (%d, %d) %d\n", cursor.x, cursor.y, i);*/
		break;
	default:
		break;
	}
	return ret;
}
