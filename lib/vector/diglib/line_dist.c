/*
* $Id$
*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Original author CERL, probably Dave Gerdes.
*               Update to GRASS 5.1 Radim Blazek.
*
* PURPOSE:      Lower level functions for reading/writing/manipulating vectors.
*
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include    <math.h>

#define ZERO(x) ((x) < tolerance && (x) > -tolerance)
#define TOLERANCE 1.0e-10
static double tolerance = TOLERANCE;

int 
dig_set_distance_to_line_tolerance (double t)
{
  if (t <= 0.0)
    t = TOLERANCE;
  tolerance = t;

  return 0;
}

/*
*   dig_distance2_point_to_line ()
*   compute square of distance of point (x,y) to line segment (x1,y1 - x2,y2)
*   ( works correctly for  x1==x2 && y1==y2 )
*
*   returns: square distance
*   sets (if not NULL): *px, *py - nearest point on segment
*                       *pdist - distance of px,py from segment start
*                       *status = 0 if ok, -1 if t < 0  and 1 if t > 1
*                                 (tells if point is w/in segment space, or past ends)
*/

double 
dig_distance2_point_to_line (
				 double x, double y,	  /* point */
				 double x1, double y1, double x2, double y2,	/* line segment */
				 double *px, double *py,  /* point on segment */
				 double *pdist,	          /* distance of point on segment from the */
							  /* first point of segment */
				 int *status)
{
    register double dx, dy;
    register double dpx, dpy;
    register double tpx, tpy;
    double t;
    int    st;

    st = 0;

    dx = x2 - x1;
    dy = y2 - y1;

    if (ZERO (dx) && ZERO (dy))	{ /* line is degenerate */
      dx = x1 - x;
      dy = y1 - y;
      tpx = x1;
      tpy = y1;
    } else {
	t = (dx * (x - x1) + dy * (y - y1)) / (dx * dx + dy * dy);

	if (t < 0.0) {			/* go to x1,y1 */
	    t = 0.0;
	    st = -1;
	} else if (t > 1.0) {		/* go to x2,y2 */
	    t = 1.0;
	    st = 1;
	}

	/* go t from x1,y1 towards x2,y2 */
	tpx = dx * t + x1;
	tpy = dy * t + y1;
	dx = tpx - x;
	dy = tpy - y;
    }

    if ( px ) *px = tpx;
    if ( py ) *py = tpy;
    if ( status ) *status = st;
    if ( pdist ) {
	dpx = tpx - x1;
	dpy = tpy - y1;
	*pdist = sqrt ( dpx * dpx + dpy * dpy ); 
    }
    
    return dx * dx + dy * dy;
}

