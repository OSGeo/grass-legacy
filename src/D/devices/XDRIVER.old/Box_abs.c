#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

extern Display *dpy;
extern Window grwin;
extern Pixmap bkupmap;
extern GC     gc;

Box_abs(x1,y1,x2,y2)
{
	int tmp ;

	if (x1 > x2)
	{
		tmp=x1 ;
		x1 = x2 ;
		x2 = tmp ;
	}
	if (y1 > y2)
	{
		tmp=y1 ;
		y1 = y2 ;
		y2 = tmp ;
	}
    XFillRectangle(dpy,grwin,gc,x1,y1,x2-x1+1,y2-y1) ;
    XFillRectangle(dpy,bkupmap,gc,x1,y1,x2-x1+1,y2-y1) ;
}
