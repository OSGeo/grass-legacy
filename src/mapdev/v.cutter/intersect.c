/**** intersect.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "cutter.h"

/*
**  This code comes from algorithms in the book  "Algorithims"  by
**   Robert Sedgewick,  Addison Wesley Publishers. 2nd Edition
**   Chapter 24.
*/

intersect (l1, l2)
    struct line_t l1, l2;
{
    return ((ccw (l1.p1, l1.p2, l2.p1) * ccw (l1.p1, l1.p2, l2.p2)) <= 0)
	&& ((ccw (l2.p1, l2.p2, l1.p1) * ccw (l2.p1, l2.p2, l1.p2)) <= 0);
}

/*
** return 1 if counter-clockwise, -1 clockwise  0 colinear sortuv 
**
** returns -1: CW   1: CCW   0: colinear (sortuv)  
** 
**  0 if  p2 is colinear w/ p0-p1 && p0 < p2 > p1  or vice-versa
**
**   1 if p2 > p1 > p0
**  -1 if p2 < p0 < p1
*/
ccw (p0, p1, p2)
    struct point_t p0, p1, p2;
{
    double dx1, dx2;
    double dy1, dy2;
    double a, b;

    dx1 = p1.x - p0.x;  dy1 = p1.y - p0.y;
    dx2 = p2.x - p0.x;  dy2 = p2.y - p0.y;

    a = dx1 * dy2;
    b = dy1 * dx2;

    if (a > b)
	return 1;

    if (a < b)
	return -1;
    
    if (dx1*dx2 < 0 || dy1*dy2 < 0)
	return -1;
    
    if ((dx1*dx1 + dy1*dy1) >= (dx2*dx2 + dy2*dy2))
	return 0;
    else
	return 1;
}

/* returns -1: LEFT   1: RIGHT   0: colinear (sortuv)   */
point_right_of_line (p0, p1, p2)
    struct point_t p0, p1, p2;
{
    return -ccw (p0, p1, p2);
}
struct foo {
    double val;
    char who;
};

static int
compar (a, b)
    struct foo *a, *b;
{
    double x;

    x = a->val - b->val;
    if (x < 0)
	return -1;
    if (x > 0)
	return 1;
    return 0;
}

/*
**
**  This code sucks.  I have 2 tribbles which intersect at the middle
**  point and are not colinear.  I want to know if they cross or just
**  run into each other and diverge again.
**   What I need is a good point_in_quarter_plane test (i.e. 
**    point_in_half_plane run twice.)
**
**  Instead, this uses the theta routine to check angles.  in increasing
**  order, we should have boy-girl-boy-girl (or vice-vs.) to have an 
**   intersection.  It even calls qsort!  ehck
*/
tribbles_intersect (A, B)
    struct tribble A, B;
{
    register int i;
    struct foo a[4];

/*    return ((ccw (A.p1, A.p2, B.p1) * ccw (A.p1, A.p2, B.p3)) <= 0); */


    a[0].val = theta (A.p2, A.p1);
    a[1].val = theta (A.p2, A.p3);
    a[2].val = theta (B.p2, B.p1); 
    a[3].val = theta (B.p2, B.p3);

    a[0].who = 'A';
    a[1].who = 'A';
    a[2].who = 'B';
    a[3].who = 'B';

    qsort ((char *) a, 4, sizeof (struct foo), compar);

    for (i = 1 ; i <= 3 ; i++)
	if (a[i-1].who == a[i].who)
	    return 0;

    return 1;
}
