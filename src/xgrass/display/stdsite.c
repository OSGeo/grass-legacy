
#include "xgdisp.h"


void
#ifdef _NO_PROTO
InitStdSiteAttributes(dpy,scrn,gc)
    Display *dpy;
    int	scrn;
    GC	gc;
#else
InitStdSiteAttributes(Display *dpy, int scrn, GC gc)
#endif
{
    unsigned long mask;
    XGCValues gcv;

    mask = GCFont;
    XGetGCValues(dpy,gc,mask,&gcv);

    Global.stdsiteattr.icontype = CROSS;
    Global.stdsiteattr.size     = 5;		
    Global.stdsiteattr.width    = 1;		
    Global.stdsiteattr.color    = XgdGetVectColorPixelByName("black");
}

