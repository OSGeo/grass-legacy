#include "xgrass_dlib.h"

int
#ifdef _NO_PROTO
_XgdGetTextLength(dpy, gc, name)
  Display  *dpy;
  GC        gc;
  char     *name;
#else
_XgdGetTextLength(Display * dpy, GC gc, char *name)
#endif
{
  XFontStruct    *font_info;
  int       width;

  font_info = XQueryFont(dpy, XGContextFromGC(gc));

  width = XTextWidth(font_info, name, strlen(name));
  return (width);
}



#ifdef _NO_PROTO
_XgdGetTextAscentDescent(dpy, gc, name, ascent, descent)
  Display  *dpy;
  GC        gc;
  char     *name;
  int      *ascent;
  int      *descent;
#else
_XgdGetTextAscentDescent(
     Display * dpy,
     GC gc,
     char *name,
     int *ascent,
     int *descent)
#endif
{
  XFontStruct    *font_info;
  int       direc, asct, desct;

  XCharStruct     charst;

  font_info = XQueryFont(dpy, XGContextFromGC(gc));

  XTextExtents(font_info, name, strlen(name),
         &direc, &asct, &desct, &charst);

  *ascent = asct;
  *descent = desct;
}
