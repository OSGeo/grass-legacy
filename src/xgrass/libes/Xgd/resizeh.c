#include "xgrass_dlib.h"

void
#ifdef _NO_PROTO
XgdDrawResizeHandles(obj, gc)
XgdObject *obj;
GC gc;
#else
XgdDrawResizeHandles(XgdObject *obj, GC gc)
#endif
{
    if ( obj == NULL ) {
        XgdError("NULL object passed to XgdDrawResizeHandles");
    }

    /* upper left */
    XDrawRectangle(obj->display, obj->window, gc, obj->x - 3, obj->y - 3, 
        6, 6);
    /* top */
    XDrawRectangle(obj->display, obj->window, gc, 
        obj->x + obj->width/2 - 3, obj->y - 3, 6, 6);
    /* upper right */
    XDrawRectangle(obj->display, obj->window, gc, obj->x + obj->width - 3, 
        obj->y - 3, 6, 6);
    /* right */
    XDrawRectangle(obj->display, obj->window, gc, 
        obj->x + obj->width - 3, obj->y + obj->height/2 - 3, 6, 6);
    /* lower right */
    XDrawRectangle(obj->display, obj->window, gc, obj->x + obj->width - 3, 
        obj->y + obj->height - 3, 6, 6);
    /* bottom */
    XDrawRectangle(obj->display, obj->window, gc, 
        obj->x + obj->width/2 - 3, obj->y + obj->height - 3, 6, 6);
    /* lower left */
    XDrawRectangle(obj->display, obj->window, gc, obj->x - 3, 
        obj->y + obj->height - 3, 6, 6);
    /* left */
    XDrawRectangle(obj->display, obj->window, gc, 
        obj->x - 3, obj->y + obj->height/2 - 3, 6, 6);
}

int
#ifdef _NO_PROTO
XgdInResizeHandle(obj, x, y)
XgdObject *obj;
int x, y;
#else
XgdInResizeHandle(XgdObject *obj, int x, int y)
#endif
{
    int i;

    for ( i = 0; i < 8; i++ ) {
        if ( XgdPointInBox(obj->handles[i], x, y) ) return i;
    }
    return XGD_OUTSIDE_HANDLE;
}
