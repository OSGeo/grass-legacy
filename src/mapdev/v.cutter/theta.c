/**** theta.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include <math.h>
#include "cutter.h"
/*
**  This code comes from algorithms in the book  "Algorithims"  by
**   Robert Sedgewick,  Addison Wesley Publishers. 2nd Edition
**   Chapter 24.
**
**   return a number from 0 to 360 which has nothing to do w/ degrees
**   angle, but has same property.  Basically is sposed to be faster and
**   less prone to exceptions.
*/

double
theta (p1, p2)
    struct point_t p1;
    struct point_t p2;
{
    double dx, dy;
    double ax, ay;
    double t;

    dx = p2.x - p1.x;   ax = fabs (dx);
    dy = p2.y - p1.y;   ay = fabs (dy);

    if (!dx && !dy)
	t = 0.;
    else
	t = dy / (ax + ay);

    if (dx < 0)
	t = 2 - t;
    else if (dy < 0)
	t += 4;

    return t * 90.;
}
