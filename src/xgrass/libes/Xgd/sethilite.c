#include "xgrass_dlib.h"

Pixel 
#ifdef _NO_PROTO
XgdSetHighlightColor(dpy, color)
        Display        *dpy;
        char           *color;
#else
XgdSetHighlightColor(
                     Display * dpy,
                     char *color)
#endif
{
        XColor          colr;

        if ( __XGDMonochrome ) return WhitePixel(dpy, DefaultScreen(dpy));

        XParseColor(dpy, __XGDColormap, color, &colr);
        __XGDHighlight = __XGDLut[0].pixel;
        __XGDLut[0].status = XG_LUT_STATUS_NO_FREE;
        __XGDLut[0].accesses++;
        __XGDPrivateCellsLeft--;
        return (__XGDLut[0].pixel);
}

Pixel 
#ifdef _NO_PROTO
XgdGetHighlightColor()
#else
XgdGetHighlightColor( void)
#endif
{
    return __XGDHighlight;
}
