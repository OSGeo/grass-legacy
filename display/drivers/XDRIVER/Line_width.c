#include "includes.h"

static int linewidth = 0;

int XD_Line_width(int width)
{
	int ret = linewidth;
	XGCValues gc_values;

	if(width < 0)
		return ret;

	gc_values.line_width = (width < 0 ? 0 : width);
	gc_values.cap_style = CapRound;
	XChangeGC(dpy, gc, GCLineWidth|GCCapStyle, &gc_values);

	linewidth = width;

	return ret;
}

