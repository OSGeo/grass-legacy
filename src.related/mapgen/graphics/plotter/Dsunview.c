#include <varargs.h>
#include "plotter.h"

/***************   SUNVIEW CODE *******************/
#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <sunwindow/cms_rainbow.h>


static Frame		frame;
static Canvas		canvas;
static Pixwin 		*pw;
extern Notify_error notify_dispatch();



/* canvs procs */
static void	Graph_Set();
static void     draw_line();
static void	Graph_Close();
static void	Erase();
/***************   END SUNVIEW CODE *******************/


static char s[200] = "\0";		/* required structure for return */
static XYS cursor = {0, 0, s};	/* data.  "s" may be larger, if needed */

	/* basic screen ranges */
# define XPMAX	799
# define YPMAX	599

static int pen = 1;


XYS *Dsunview(va_alist) va_dcl {
	va_list vap;
	int i, x, y, cmd, xv, yv;
	static int xvl, yvl;
	XYS *ret = &cursor;

	va_start(vap); cmd = va_arg(vap, int);
/*DEBUG fprintf (stderr, "Dsunview %d\n",cmd);*/
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
		Graph_Set();
		xvl = yvl = 0;
		{ cursor.y = YPMAX; cursor.x = XPMAX; }
		cursor.x = cursor.x / Dglobal.scale;
		cursor.y = cursor.y / Dglobal.scale;
		break;
	case D_DONE:

	case D_PANIC:
		Graph_Close();
		break;
	case D_DISABL:
		Graph_Close();
		break;
	case D_MOVE:
		xvl = va_arg(vap, long) * Dglobal.scale;
		yvl = va_arg(vap, long) * Dglobal.scale;
/*DEBUG   fprintf (stderr, "in move (%d,%d)\n", xvl, yvl); */
		break;
	case D_LINE:
		xv = va_arg(vap, long) * Dglobal.scale;
		yv = va_arg(vap, long) * Dglobal.scale;
/*DEBUG   fprintf (stderr, "in line (%d,%d) (%d,%d) (%d,%d)\n %lf\n", XPMAX, YPMAX, xvl, yvl, xv, yv, Dglobal.scale); */
		draw_line(xvl, YPMAX-yvl, xv, YPMAX-yv, pen);
		xvl = xv;
		yvl = yv;
		break;
	case D_ERASE:
		Erase();
		break;
	case D_PEN:
		pen = va_arg(vap, long);
/*DEBUG fprintf(stderr,"D_PEN: %d\n",pen);*/
		break;
	case D_CURSOR:
		cursor.x = xvl;
		cursor.y = YPMAX- yvl;
/*DEBUG fprintf (stderr, "in cursor (%d, %d)\n", cursor.x, cursor.y);*/
/*		Get_location_with_pointer(&cursor.x, &cursor.y, &i);*/
/*DEBUG fprintf (stderr, "AFTER cursor (%d, %d) %d\n", cursor.x, cursor.y, i);*/
		cursor.y = YPMAX - cursor.y;
/*		gets(s);*/
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

/*** Set up some standard colors ***************/
static void set_standardcolor(red,green,blue)
    unsigned char red[];
    unsigned char green[];
    unsigned char blue[];
{
/*	WHITE*/		red[0] = 255;	green[0] = 255;	blue[0]= 255 ;
/*	BLACK*/		red[1] =  0;	green[1] = 0;	blue[1]=  0 ;
/*	MAGENTA*/	red[2] =255;	green[2] = 0;	blue[2]=127 ;
/*	GREEN*/		red[3] = 0;	green[3] = 255;	blue[3]= 0 ;
/*	VIOLET*/	red[4] = 255;	green[4] = 0;	blue[4]= 255 ;
/*	BLUE*/		red[5] = 0;	green[5] = 0;	blue[5]= 255 ;
/*	YELLOW*/	red[6] = 255;	green[6] = 255;	blue[6]= 0 ;
/*	ORANGE*/	red[7] = 255;	green[7] =127;	blue[7]= 0 ;
/*	RED*/		red[8] = 255;	green[8] = 0;	blue[8]= 0 ;
}



/***  Code to setup and use a SUNVIEW canvas   **************/
static void Graph_Set()
{
char *cmsname = "MAPGEN";
    unsigned char red[CMS_RAINBOWSIZE];
    unsigned char green[CMS_RAINBOWSIZE];
    unsigned char blue[CMS_RAINBOWSIZE];
/*DEBUG fprintf(stderr,"In Graph_Set\n");*/

    frame =
	window_create(NULL, FRAME,
	    FRAME_LABEL,    	"MAPGEN",
	    WIN_BOTTOM_MARGIN,	0,
	    WIN_RIGHT_MARGIN,	0,
	    WIN_X,		0,
	    WIN_Y,		0,
	    0);


    canvas = 
        window_create(frame, CANVAS,
	    CANVAS_RETAINED,	TRUE,
	    CANVAS_HEIGHT,  	YPMAX+10,
	    CANVAS_WIDTH,	XPMAX+10,
	    WIN_HEIGHT,  	YPMAX+20,
	    WIN_WIDTH,		XPMAX+20,
	    CANVAS_FIXED_IMAGE, TRUE,
	    CANVAS_AUTO_SHRINK,	FALSE,
	    CANVAS_AUTO_EXPAND,	FALSE,
	    CANVAS_MARGIN,	5,
	    WIN_X,		0,
	    WIN_Y,		0,
	    0);

  

    cms_rainbowsetup(red,green,blue);
    pw = (Pixwin *) canvas_pixwin(canvas);
/*DEBUG fprintf(stderr,"pw= %ld\n",pw);*/
    pw_setcmsname(pw, cmsname);
/*    set_standardcolor(red,green,blue);*/
    pw_putcolormap(pw, 0, CMS_RAINBOWSIZE, red, green, blue);
    window_fit(frame);
    return;
}

/* canvas procs */

/* utilities */
static void
draw_line(x1, y1, x2, y2, c)
int	x1, y1, x2, y2, c;
{
/*DEBUG int x,y;
x = (int) window_get(canvas,CANVAS_WIDTH);
y = (int) window_get(canvas,CANVAS_HEIGHT);
fprintf(stderr,"%ld %d %d %d %d - %d %d\n", pw, x, y, x1, y1, x2, y2);*/
   (void)pw_vector(pw, x1, y1, x2, y2, PIX_SRC, c);
}


/** Close the drawing canvas ****/
static void Graph_Close()
{
window_main_loop(frame);
window_done(frame);
}


/** Erase pixwin ****/
static void Erase()
{
(void)pw_writebackground(pw,0,0,XPMAX+2,YPMAX+2,PIX_SRC);
}
