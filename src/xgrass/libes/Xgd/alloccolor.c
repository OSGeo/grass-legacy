#include "xgrass_dlib.h"

Status
#ifdef _NO_PROTO
_XgdAllocCells(dpy, xcolor)
        Display        *dpy;
        XColor         *xcolor;
#else
_XgdAllocCells(Display dpy,
               XColor * xcolor)
#endif
{
    unsigned long   pixel;
    int             result;

    result = XAllocColorCells(dpy, __XGDColormap, False, NULL, 0, &pixel, 1);
    if (result == 0) return False;
    xcolor->pixel = pixel;
    XStoreColor(dpy, __XGDColormap, xcolor);
    return True;
}
