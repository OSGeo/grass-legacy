/**** linecros.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include <stdio.h>
#include "math.h"

#define lessor(a,b) ((a) < (b) ? (a) : (b))
/***************************************************************
* test_for_intersection (ax1,ay1,ax2,ay2,bx1,by1,bx2,by2)
*   double ax1,ax2,ay1,ay2;
*   double bx1,bx2,by1,by2;
*
* returns
*   0 no intersection at all
*   1 the line segments intersect at only one point
*  -1 the line segments intersect at many points, i.e., overlapping
*     segments from the same line
*
* find_intersection (ax1,ay1,ax2,ay2,bx1,by1,bx2,by2,x,y)
*   double ax1,ax2,ay1,ay2;
*   double bx1,bx2,by1,by2;
*   double *x,*y;
*
* returns
*   0 no intersection
*   1 x,y set to (unique) intersection	RIGHT to LEFT
*   2 x,y set to (unique) intersection	LEFT  to RIGHT
*  -1 lines overlap, no unique intersection
*
* Based on the following:
*
*    (ax2-ax1)r1 - (bx2-bx1)r2 = ax2 - ax1
*    (ay2-ay1)r1 - (by2-by1)r2 = ay2 - ay1
*
* Solving for r1 and r2, if r1 and r2 are between 0 and 1,
* then line segments (ax1,ay1)(ax2,ay2) and (bx1,by1)(bx2,by2)
* intersect
****************************************************************/

#define D  ((ax2-ax1)*(by1-by2) - (ay2-ay1)*(bx1-bx2))

#define D1 ((bx1-ax1)*(by1-by2) - (by1-ay1)*(bx1-bx2))

#define D2 ((ax2-ax1)*(by1-ay1) - (ay2-ay1)*(bx1-ax1))

test_for_intersection (ax1,ay1,ax2,ay2,bx1,by1,bx2,by2)
    double ax1,ax2,ay1,ay2;
    double bx1,bx2,by1,by2;
{
    register double d, d1, d2;
    double t;

    d  = D;
    d1 = D1;
    d2 = D2;

    if (d > 0)
	return (d1 >= 0 && d2 >= 0 && d >= d1 && d >= d2);
    if (d < 0)
	return (d1 <= 0 && d2 <= 0 && d <= d1 && d <= d2);

/* lines are parallel */
    if (d1 || d2)
	return 0;

/* segments are colinear. check for overlap */
    if (ax1 > ax2)
    {
	t=ax1;
	ax1=ax2;
	ax2=t;
    }
    if (bx1 > bx2)
    {
	t=bx1;
	bx1=bx2;
	bx2=t;
    }
    if (ax1 > bx2) return 0;
    if (ax2 < bx1) return 0;

/* there is overlap */

    if (ax1 == bx2 || ax2 == bx1)
	return 1; /* endpoints only */
    return -1;    /* true overlap   */
}


find_intersection (ax1,ay1,ax2,ay2,bx1,by1,bx2,by2,x,y)
    double ax1,ax2,ay1,ay2;
    double bx1,bx2,by1,by2;
    double *x, *y;
{
    register double d, r1,r2;
    double t;


    d  = D;

/*DEBUG*/ if (fabs (d) < 1.) debugf ("D = %lf   ", d);
    if (d)
    {

	r1 = D1/d;
	r2 = D2/d;
	if (r1 < 0 || r1 > 1 || r2 < 0 || r2 > 1)
	{
	    return 0;
	}
	*x = ax1 + r1 * (ax2 - ax1);
	*y = ay1 + r1 * (ay2 - ay1);
	return 1;
    }
    else
    {
/*DEBUG*/ debugf ( "D = 0\n");
    }

/* lines are parallel */
    if (D1 || D2)
    {
	return 0;
    }

/* segments are colinear. check for overlap */
    if (ax1 > ax2)
    {
	t=ax1;
	ax1=ax2;
	ax2=t;
    }
    if (bx1 > bx2)
    {
	t=bx1;
	bx1=bx2;
	bx2=t;
    }
    if (ax1 > bx2) return 0;
    if (ax2 < bx1) return 0;

/* there is overlap */

    if (ax1 == bx2)
    {
	*x = ax1;
	*y = ay1;
	return 1; /* endpoints only */
    }
    if(ax2 == bx1)
    {
	*x = ax2;
	*y = ay2;
	return 1; /* endpoints only */
    }
    return -1;    /* overlap, no single intersection point */
}
