
/* @(#)main.c	2.2   7/24/87 */

/* the following defines must be included for all drivers */
/* modified for multiple paper sizes */
#include <varargs.h>
#include "plotter.h"
#define TEMP "/tmp/GRASS"
#define MAXPEN 256




#include <stdio.h>
#include "gis.h"


static char s[200] = "\0";		/* required structure for return */
static XYS cursor = {0, 0, s};	/* data.  "s" may be larger, if needed */
	/* basic screen ranges */
int XPMAX = 600;
int YPMAX = 500;
int t, b, l, r;

static void plot_all(c,x,y)
char c;
int x, y;

{
	 switch (c) {
		case 'c':
			R_color(x);
			break;
		case 'm':
			R_move_abs(x,y);
			break;
		case 'l':
			R_cont_abs(x,y);
			break;
		}
}

static void initGRASS()
{
int cr,cb,cg,i;
FILE *colorfp, *fopen();
struct Gcolortable {
	int r,g,b;
	} ctable[MAXPEN];

/* Initialize the GIS calls */
	G_gisinit("MAPGEN") ;



/* Make sure map is available */

    R_open_driver();



/* Read in the map window associated with window */


/* Determine window */
	
	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("Getting screen window") ;
	XPMAX = r - l +1;
	YPMAX = b - t +1;
/* DEBUG  fprintf(stderr,"t%d b%d l%d r%d\n",t,b,l,r);*/

/* Check for and read dig header info */


/*read color table if present*/
if ((colorfp = fopen(".colortable","r")) == NULL) {
/* Set color */
	R_reset_color(255,255,255,1);
	R_reset_color(255,0,127,2);
	R_reset_color(0,0,255,3);
	R_reset_color(222,43,242,4);
	R_reset_color(0,255,0,5);
	R_reset_color(255,255,0,6);
	R_reset_color(255,127,0,7);
	R_reset_color(255,0,0,8);
	R_reset_color(255,255,127,9);
	R_reset_color(255,0,255,10);
	R_reset_color(0,255,255,11);
	R_reset_color(255,127,255,12);
	R_reset_color(127,255,255,13);
	R_reset_color(127,255,127,14);
	R_reset_color(255,127,127,15);
	R_reset_color(127,127,255,16);
	}
else {
	while (fscanf(colorfp,"%d %d %d %d",&cr,&cg,&cb,&i) != EOF){
		if (i < 256) {
		ctable[i].r = cr;
		ctable[i].g = cg;
		ctable[i].b = cb;
		}
	}
	for (i=0;i<256;i++) 
		R_reset_color(ctable[i].r,ctable[i].g,ctable[i].b,i);
	}
}

static void endGRASS()
{
	R_flush();
    R_close_driver();
}


	XYS *
DGRASS(va_alist) va_dcl {
	va_list vap;
	int i, pen, xv, yv, cmd;
	XYS *ret = &cursor;
	char m;
	FILE *scalefp;

	va_start(vap); cmd = va_arg(vap, int);
/*DEBUG fprintf (stderr, "DGRASS %d\n",cmd);*/
	switch(cmd) {
	case D_INIT:
		pen = 1;
		initGRASS();
		plot_all('c',1);
	case D_SCALE:
	if ((scalefp = fopen(".d_scale","w")) == NULL) {
		fprintf(stderr,"DGRASS: Warning can not open scale file\n");
		fprintf(stderr,"DGRASS: call you system adminitrator\n");
		}
	else {
		fprintf(scalefp,"%lf",Dglobal.scale);
		fclose(scalefp);
		}
/*DEBUG fprintf(stderr,"SCALE %lf\n",Dglobal.scale);*/
		if (Dglobal.scale <= 0.)
			Dglobal.scale = 1.;
		{ cursor.y = YPMAX; cursor.x = XPMAX; }
		cursor.x = cursor.x / Dglobal.scale + 1;
		cursor.y = cursor.y / Dglobal.scale + 1;
		break;

	case D_PANIC:
/*DEBUG fprintf (stderr, "in panic\n");*/
	case D_DISABL:
/*DEBUG fprintf (stderr, "in disable\n");*/
	case D_DONE:
		endGRASS();
		break;

	case D_MOVE:
		m = 'm';
		goto draw;

	case D_LINE:
		m = 'l';
draw:		xv = va_arg(vap, long) * Dglobal.scale;
		yv = va_arg(vap, long) * Dglobal.scale;
		yv = YPMAX - yv + t -1;
		xv = xv + l -1;
		plot_all(m,xv,yv);
/*DEBUG   fprintf (stderr, "in line (%d,%d) (%d,%d)\n %lf\n", XPMAX, YPMAX,  xv, yv, Dglobal.scale); */
		break;
	case D_ERASE:
/*DEBUG fprintf (stderr, "in erase \n", xv, yv);*/
		break;
	case D_PEN:
		pen = va_arg(vap, long);
/*DEBUG fprintf (stderr, "in pen %d \n", pen);*/
		while (pen > MAXPEN) pen -= MAXPEN;
/*DEBUG fprintf (stderr, "in pen %d \n", pen);*/
		plot_all('c',pen);
		break;
	case D_CURSOR:
/*DEBUG fprintf (stderr, "in cursor (%d, %d)\n", cursor.x, cursor.y);*/
		break;
	default:
		break;
	}
	return ret;
}

