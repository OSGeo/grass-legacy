#include "sun.h"
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
/*
 * draw a line between two given points in the current color. X version
 *
 * Called by:
 *     Cont_abs() in ../lib/Cont_abs.c
 */

extern int SCREEN_BOTTOM ;

extern Display *dpy;
extern Window  grwin;
extern Pixmap  bkupmap;
extern GC      gc;

draw_line(cur_x, cur_y, x, y)
int cur_x,cur_y,x,y;
{

    int ulx,uly;
    unsigned wid,hit;

	XDrawLine(dpy,grwin,gc, cur_x, cur_y, x, y) ;
	XDrawLine(dpy,bkupmap,gc, cur_x, cur_y, x, y) ;
	sun_x = x ;
	sun_y = y ;
}
