/* -*-c-basic-offset: 4; -*- */

#include "includes.h"
/* draw a line between two given points in the current color. X version
 * 
 * Called by: Cont_abs() in ../lib/Cont_abs.c */


#include "layers.h"

extern Display *dpy;
extern Window grwin;
extern GC gc;
extern Pixmap bkupmap;
extern int backing_store;


int draw_line (int cur_x, int cur_y, int x, int y)
{
	layer_t *s;
	s=get_scratch_layer();
	s->flags |= TRANSPARENT;
	XDrawLine(dpy, s->l, s->gc, cur_x, cur_y, x, y);
	XDrawLine(dpy, s->m, s->mgc, cur_x, cur_y, x, y);

 /*  	    XDrawLine(dpy, grwin, gc, cur_x, cur_y, x, y); */
   if (!backing_store)
        XDrawLine(dpy, bkupmap, gc, cur_x, cur_y, x, y);

    return 0;
}


