
/*
**  Written by Bill Brown   Fall, 1992 
**  US Army Construction Engineering Research Lab
*/


/*
** Copyright USA CERL 1992. All rights reserved.
*/

#include "digit.h"
#include <math.h>

#define X 0
#define Y 1

/**********************************************************************/

double
distance (fromx, fromy, tox, toy)
    double fromx, fromy, tox, toy;
{
    double x, y;
    x = fromx - tox;
    y = fromy - toy;
    return  sqrt (x*x + y*y);
}

/**********************************************************************/

double
distancep (from, to)
    double from[2], to[2];
{
    double x, y;
    x = from[X] - to[X];
    y = from[Y] - to[Y];
    return  sqrt (x*x + y*y);
}

/**********************************************************************/
/* returns distance squared (faster) */

double
distance2 (fromx, fromy, tox, toy)
    double fromx, fromy, tox, toy;
{
    double x, y;
    x = fromx - tox;
    y = fromy - toy;
    return (x*x + y*y);
}

/**********************************************************************/
/* returns distance squared (faster) */

double
distance2p (from, to)
    double from[2], to[2];
{
    double x, y;
    x = from[X] - to[X];
    y = from[Y] - to[Y];
    return (x*x + y*y);
}

/**********************************************************************/

double
dot_product2(a, b)
double a[2], b[2];
{
    return(a[X]*b[X] + a[Y]*b[Y]);
}

/***********************************************************************/
/* put normalized vector from p1 to p2 in dir */

get_direction(dir, p1, p2, len)
double dir[2], len;
double p1[2], p2[2];
{
    dir[X] = (p2[X] - p1[X])/len;
    dir[Y] = (p2[Y] - p1[Y])/len;

}

/************************************************************************/
/* uses semi_circumference formula */

get_circle_rad(arcpts, rad)
double arcpts[3][2], *rad;
{
double la, lb, lc, s, K;
     
    la = distance(arcpts[0][X], arcpts[0][Y], arcpts[2][X], arcpts[2][Y]);   
    lb = distance(arcpts[1][X], arcpts[1][Y], arcpts[2][X], arcpts[2][Y]);   
    lc = distance(arcpts[1][X], arcpts[1][Y], arcpts[0][X], arcpts[0][Y]);   

    s = (la + lb + lc)/2.0;
    K = sqrt(s*(s-la)*(s-lb)*(s-lc));

    *rad = la * lb * lc / (4.0 * K);

}

/************************************************************************/
/* returns the center & radius of the circle passing through arcpts    */


get_circle(arcpts, center, rad)
double arcpts[3][2], center[2], *rad;
{
double  dx, ma, mb, mn, bn;
double mida[2], midb[2], maspoke, mbspoke, baspoke, bbspoke;
int maspokeinf, mbspokeinf;

    maspokeinf = mbspokeinf = 0; 
   
    mida[X] = (arcpts[0][X] + arcpts[1][X])/ 2.0;
    mida[Y] = (arcpts[0][Y] + arcpts[1][Y])/ 2.0;
    midb[X] = (arcpts[2][X] + arcpts[1][X])/ 2.0;
    midb[Y] = (arcpts[2][Y] + arcpts[1][Y])/ 2.0;
    
    /* check for zero division, infinite slope! */
    dx = arcpts[0][X] - arcpts[1][X];
    if(dx){
	ma = (arcpts[0][Y] - arcpts[1][Y])/dx; 
	if(ma)
	    maspoke = -1.0/ma;
	else 
	    maspokeinf = 1;
    }
    else
	maspoke = 0.0;
    baspoke = mida[Y] - maspoke * mida[X];

    dx = arcpts[2][X] - arcpts[1][X];
    if(dx){
	mb = (arcpts[2][Y] - arcpts[1][Y])/dx; 
	if(mb)
	    mbspoke = -1.0/mb;
	else
	    mbspokeinf = 1;
    }
    else
	mbspoke = 0.0;
    bbspoke = midb[Y] - mbspoke * midb[X];

    if(maspokeinf){
	center[X] = mida[X];
	center[Y] = mbspoke*center[X] + bbspoke;
    }
    else if(mbspokeinf){
	center[X] = midb[X];
	center[Y] = maspoke*center[X] + baspoke;
    }
    else{
	mn = maspoke - mbspoke;
	bn = baspoke - bbspoke;
	center[X] = -bn/mn;
	center[Y] = mbspoke*center[X] + bbspoke;
    }

    *rad = distance(center[X], center[Y], arcpts[0][X], arcpts[0][Y]);

}


/**********************************************************************/
/* cos of the angle between two vectors is (a . b)/|a||b| */

double
v2angle(v1, v2, mag1, mag2)
double v1[2], v2[2];
double mag1, mag2;
{
double costheta;

    costheta = (v1[X] * v2[X] + v1[Y] * v2[Y])/(mag1 * mag2);
    return(acos(costheta)); 

}


/**********************************************************************/
/* cos of the angle between two vectors is (a . b)/|a||b| 
   This one returns the angle from 0 -> 2*pi clockwise from 12 o'clock, 
   using the second point in the vector as the clock center */

double
clockangle(pt1, pt2, mag)
double pt1[2], pt2[2];
double mag;
{
double theta, costheta;

    costheta = (pt1[Y] - pt2[Y])/mag;
    theta = acos(costheta); 

    if(pt1[X] < pt2[X])
	theta = 2.0 * Pi - theta;

    return(theta); 

}


/************************************************************************/

normalize_vector2(v)
double v[2];
{
double norm;

    norm = sqrt(dot_product2(v, v));
    v[0] /= norm;
    v[1] /= norm;

}

/************************************************************************/

copy_pnt2(a, b)
double a[2], b[2];
{
    a[0] = b[0];
    a[1] = b[1];
}

/************************************************************************/

/*
 * Determine if coplanar circles intersect.
 *
 * If they intersect at two points, p1 and p2 hold these locations
 * and (1) is returned.
 *
 * If the circles are concentric, (2) is returned.
 *
 * If they don't intersect at all, (0) is returned.
 */
find_coplanar_circle_intersect_circle(a, b, p1, p2)
Circle *a, *b;
double p1[2], p2[2];
{
	double dir[2], tmp[2], segdir[2];
	double dist, dsq, bsq, asq, edist, chorddist;

	/* compute distance and normalized vector from center to center */
	dist = distancep(a->cen, b->cen);
	get_direction(dir, a->cen, b->cen, dist);
	/* if too far apart, no intersection */
	if (dist > a->rad + b->rad)
		return (0);
	/* concentric?  also takes care of dist=0 case */
	if (dist < EPSILON ||	/* Just in case it's teeeeeeeny */
	    dist + a->rad <= b->rad ||
	    dist + b->rad <= a->rad)
		return (2);
	/* else find two points of intersection */

	/* compute chorddist, which is signed distance from a->center
	 * along 'dir' to chord connecting intersection points */
	dsq = dist*dist;
	asq = a->rad*a->rad;
	bsq = b->rad*b->rad;
	chorddist = (dsq + asq - bsq)/(2*dist);

	/* compute chord midpoint */
	copy_pnt2(tmp, a->cen);
	tmp[X] += (chorddist*dir[X]);
	tmp[Y] += (chorddist*dir[Y]);
	/* compute distance from midpoint to ends */
	edist = asq - chorddist*chorddist;

	copy_pnt2(p1, tmp);
	copy_pnt2(p2, tmp);
	if (edist > EPSILON) {
		edist = sqrt(edist);
		/* compute chord direction */
		if(dir[Y]){
		    segdir[X] = -dir[Y];
		    segdir[Y] = dir[X];
		}
		else{
		    segdir[X] = dir[Y];
		    segdir[Y] = -dir[X];
		}
		/*
		normalize_vector2(segdir);
		*/
fprintf(stderr,"a->b direrction = %.3lf,%.3lf\n", dir[X], dir[Y]);
fprintf(stderr,"chord direrction = %.3lf,%.3lf\n", segdir[X], segdir[Y]);
		/* finally, compute points of intersection */
		p1[X] += (edist * segdir[X]);
		p1[Y] += (edist * segdir[Y]);
		p2[X] -= (edist * segdir[X]);
		p2[Y] -= (edist * segdir[Y]);
	}
	return (1);
}

/************************************************************************/




