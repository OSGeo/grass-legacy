#include "includes.h"

/* changes the width of line */
int
line_width(int width)
{
	XGCValues gc_values;

	XGetGCValues(dpy, gc, GCLineWidth|GCCapStyle, &gc_values);
	gc_values.line_width = width;
	gc_values.cap_style = CapRound;
	XChangeGC(dpy, gc, GCLineWidth|GCCapStyle, &gc_values);

	return 0;
}
