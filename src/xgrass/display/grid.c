#include "xgdisp.h"


void
#ifdef _NO_PROTO
InitGridAttributes(dpy,scrn,gc)
    Display *dpy;
    int	scrn;
    GC	gc;
#else
InitGridAttributes(Display *dpy, int scrn, GC gc)
#endif
{
    unsigned long mask;
    XGCValues gcv;

    mask = GCFont;
    XGetGCValues(dpy,gc,mask,&gcv);

    Global.gridattr.labelon	= 0;
    Global.gridattr.gridgap     = 1000.0;
    Global.gridattr.spacing     = 5;
    Global.gridattr.color       = XgdGetVectColorNameByPixel("black");
    Global.gridattr.linewidth   = 0;
    Global.gridattr.linepattern = XGD_LINE_PATTERN_SOLID;
    Global.gridattr.fid	     = gcv.font;
    Global.gridattr.textcolor   = XgdGetVectColorNameByPixel("black"); 
    Global.gridattr.xoff	= 0;
    Global.gridattr.yoff	= 0;
}

