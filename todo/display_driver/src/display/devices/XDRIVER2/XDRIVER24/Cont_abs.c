#include "includes.h"
#include "../lib/driver.h"
#include "layers.h"

extern Display *dpy;
extern Window grwin;
extern GC gc;
extern Pixmap bkupmap;
extern int backing_store;


int Cont_abs (int x, int y)
{
	layer_t *s;
	s=get_scratch_layer();
	s->flags |= TRANSPARENT;
	XDrawLine(dpy, s->l, s->gc, cur_x, cur_y, x, y);
	XDrawLine(dpy, s->m, s->mgc, cur_x, cur_y, x, y);

/*      XDrawLine(dpy, grwin, gc, cur_x, cur_y, x, y); */
    if (!backing_store)
        XDrawLine(dpy, bkupmap, gc, cur_x, cur_y, x, y);
    cur_x = x;
    cur_y = y;

    return 0;
}

int Cont_rel (int x, int y)
{
	layer_t *s;
	s=get_scratch_layer();
	s->flags |= TRANSPARENT;
	XDrawLine(dpy, s->l, s->gc,cur_x, cur_y, cur_x + x, cur_y + y);
	XDrawLine(dpy, s->m, s->mgc,cur_x, cur_y, cur_x + x, cur_y + y);

/*      XDrawLine(dpy, grwin, gc, cur_x, cur_y, cur_x + x, cur_y + y); */
    if (!backing_store)
        XDrawLine(dpy, bkupmap, gc, cur_x, cur_y, cur_x + x, cur_y + y);
    cur_x += x;
    cur_y += y;

    return 0;
}

/*** end cont_abs.c ***/
