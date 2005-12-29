#include "enforce.h"


/* taken from src/mapdev/diglib/line_dist.c - TODO: fix for lat_lon */

static double tolerance = 1.0e-10;

#define ZERO(x) ((x) < tolerance && (x) > -tolerance)


#if 0
/* returns distance squared - works correctly for  x1==x2 && y1==y2 */
double distance2_point_to_seg (double x, double y, double x1, 
                double y1, double x2, double y2)
{
    double dx = x2 - x1;
    double dy = y2 - y1;
    double t;

    if (ZERO(dx) && ZERO(dy)) /* line is degenerate */
    {
	dx = x1 - x;
	dy = y1 - y;

	/* compute distance x,y to x1,y1 */
	return (SQR(dx) + SQR(dy));
    }

    t = (dx * (x - x1) + dy * (y - y1)) / (SQR(dx) + SQR(dy));

    if (t < 0.0)		/* go to x1,y1 */
        t = 0.0;
    else if (t > 1.0)		/* go to x2,y2 */
        t = 1.0;

    /* go t from x1,y1 towards x2,y2 */
    dx = dx * t + x1 - x;
    dy = dy * t + y1 - y;

    return (SQR(dx) + SQR(dy));
}
#endif


/*
 * same as distance2_point_to_seg() except px,py are pointers to
 * doubles and the line intersection is returned in them
 */
double xy_distance2_point_to_seg(double *px, double *py, double x1, 
                    double y1, double x2, double y2)
{
    double dx = x2 - x1;
    double dy = y2 - y1;
    double x = *px;
    double y = *py;
    double t;

    if (ZERO(dx) && ZERO(dy)) /* line is degenerate */
    {
        dx = x1 - x;
        dy = y1 - y;

        *px = x + dx;
        *py = y + dy;

        return (SQR(dx) + SQR(dy));
    }

    t = (dx * (x - x1) + dy * (y - y1)) / (SQR(dx) + SQR(dy));

    if (t < 0.0)		/* go to x1,y1 */
        t = 0.0;
    else if (t > 1.0)		/* go to x2,y2 */
        t = 1.0;

    /* go t from x1,y1 towards x2,y2 */
    dx = dx * t + x1 - x;
    dy = dy * t + y1 - y;

    *px = x + dx;
    *py = y + dy;

    return (SQR(dx) + SQR(dy));
}


/*
 * same as xy_distance2_point_to_seg() except returns value in status
 * telling if point is w/in segment space, or past ends
 *   
 *  status = 0 if ok, -1 if t < 0  and 1 if t > 1
 */
double xy_distance3_point_to_seg(double *px, double *py, double x1,
                double y1, double x2, double y2, int *status)
{
    double dx = x2 - x1;
    double dy = y2 - y1;
    double x = *px;
    double y = *py;
    double t;

    *status = 0;

    if (ZERO(dx) && ZERO(dy)) /* line is degenerate */
    {
        dx = x1 - x;
        dy = y1 - y;

        *px = x + dx;
        *py = y + dy;

        return (SQR(dx) + SQR(dy));
    }

    t = (dx * (x - x1) + dy * (y - y1)) / (SQR(dx) + SQR(dy));

    if (t < 0.0)		/* go to x1,y1 */
    {
        t = 0.0;
        *status = -1;
    } else if (t > 1.0) 	/* go to x2,y2 */
    {
        t = 1.0;
        *status = 1;
    }

    /* go t from x1,y1 towards x2,y2 */
    dx = dx * t + x1 - x;
    dy = dy * t + y1 - y;

    *px = x + dx;
    *py = y + dy;

    return (SQR(dx) + SQR(dy));
}


#if 0
/* returns distance squared (faster) */
double distance2(double fromx, double fromy, double tox, double toy)
{
    double x = fromx - tox;
    double y = fromy - toy;

    return (SQR(x) + SQR(y));
}
#endif
