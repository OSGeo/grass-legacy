#include <math.h>
#include "gis.h"
static double min4(double, double, double, double);
static double min2(double, double);
static double dabs (double);

/* WARNING: this code is preliminary and may be changed,
 * including calling sequences to any of the functions
 * defined here
 */

static int projection = 0;
static double factor = 1.0;


/*!
 * \brief begin distance calculations
 *
 * Initializes the distance calculations. It is used both for the
 * planimetric and latitude-longitude projections.
 * It returns 2 if the projection is latitude-longitude, 1 if the projection is
 * planimetric, and 0 if the projection doesn't hav e a metric (e.g. imagery.)
 *
 *  \param void
 *  \return int
 */

int G_begin_distance_calculations()
{
    double a, e2;

    factor = 1.0;
    switch (projection = G_projection())
    {
    case PROJECTION_LL:
	G_get_ellipsoid_parameters (&a, &e2);
	G_begin_geodesic_distance (a, e2);
	return 2;
    default:
	factor = G_database_units_to_meters_factor();
	if (factor <= 0.0)
	{
	    factor = 1.0;          /* assume meter grid */
	    return 0;
	}
	return 1;
    }
}


/*!
 * \brief distance in meters
 *
 * This routine computes the distance, in meters, from
 * <b>x1,y1</b> to <b>x2,y2.</b> If the projection is
 * latitude-longitude, this distance is measured along the geodesic. Two
 * routines perform geodesic distance calculations.
 *
 *  \param x1
 *  \param y1
 *  \param x2
 *  \param y2
 *  \return double
 */

double G_distance (double e1,double n1,double e2,double n2)
{
    double hypot();

    if (projection == PROJECTION_LL)
	return G_geodesic_distance (e1, n1, e2, n2);
    else
	return factor * hypot (e1-e2,n1-n2);
}

double
G_distance_between_line_segments (
    double ax1,double ay1,
    double ax2,double ay2,
    double bx1,double by1,
    double bx2,double by2)
{
    double ra, rb;
    double x, y;

/* if the segments intersect, then the distance is zero */
    if(G_intersect_line_segments(ax1,ay1,ax2,ay2,
			       bx1,by1,bx2,by2,
			       &ra,&rb,&x,&y) > 0)
	    return 0.0;
    return min4 (
	G_distance_point_to_line_segment (ax1,ay1,bx1,by1,bx2,by2) ,
	G_distance_point_to_line_segment (ax2,ay2,bx1,by1,bx2,by2) ,
	G_distance_point_to_line_segment (bx1,by1,ax1,ay1,ax2,ay2) ,
	G_distance_point_to_line_segment (bx2,by2,ax1,ay1,ax2,ay2)
		) ;
}

double
G_distance_point_to_line_segment (
    double xp,double yp,         /* the point */
    double x1,double y1,double x2,double y2)   /* the line segment */
{
    double dx,dy;
    double x,y;
    double xq,yq,ra,rb;
    int t;

/* define the perpendicular to the segment thru the point */
    dx = x1 - x2;
    dy = y1 - y2;

    if (dx == 0.0 && dy == 0.0)
	return G_distance (x1,y1,xp,yp);

    if (dabs(dy) > dabs(dx))
    {
	xq = xp + dy;
	yq = (dx/dy) * (xp - xq) + yp;
    }
    else
    {
	yq = yp + dx;
	xq = (dy/dx) * (yp - yq) + xp;
    }

/* find the intersection of the perpendicular with the segment */
    switch(t=G_intersect_line_segments(xp,yp,xq,yq, x1,y1,x2,y2, &ra,&rb,&x,&y))
    {
    case 0:
    case 1:
	break;
    default:
	/* parallel/colinear cases shouldn't occur with perpendicular lines */
	fprintf (stderr,"G_distance_point_to_line_segment: shouldn't happen\n");
fprintf(stderr," code=%d P=(%f,%f) S=(%f,%f)(%f,%f)\n",t,xp,yp,x1,y1,x2,y2);
	return -1.0;
    }

/* if x,y lies on the segment, then the distance is from to x,y */
    if (rb >= 0 && rb <= 1.0)
	return G_distance (x,y,xp,yp);

/* otherwise the distance is the short of the distances to the endpoints
 * of the segment
 */
    return min2(G_distance (x1,y1,xp,yp), G_distance (x2,y2,xp,yp));
}

static double dabs ( double x)
{
    return x < 0 ? -x : x;
}

static double min4(double a,double b,double c,double d)
{
    return min2 (min2(a,b), min2(c,d));
}

static double min2(double a,double b)
{
    return a < b ? a : b;
}
