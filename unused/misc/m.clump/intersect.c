#include "glob.h"

int
arc_intersects_segment (arc, x1, y1, x2, y2)
    struct arc *arc;
    double x1, y1, x2, y2;
{
    int i;
    double *x, *y;
    int n;

    x = arc->x;
    y = arc->y;
    n = arc->n;

    for (i = 1; i < n; i++, x++, y++)
	if (segments_intersect (x[0], y[0], x[1], y[1], x1, y1, x2, y2))
	    return 1;
    return 0;
}

segments_intersect (ax1, ay1, ax2, ay2, bx1, by1, bx2, by2)
    double ax1, ay1, ax2, ay2;
    double bx1, by1, bx2, by2;
{
    double ra, rb, x, y;

    return (G_intersect_line_segments (
		ax1, ay1, ax2, ay2,
		bx1, by1, bx2, by2,
		&ra, &rb, &x, &y       ) > 0);
}
