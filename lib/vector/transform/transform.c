/* @(#)transform.c	2.1  6/26/87 */
/****************************************************************
This  file  contains    routines    which    perform    (affine?)
transformations  from  one  coordinate  system  into another. The
second system may be translated, stretched, and rotated  relative
to  the  first.   The  input  system is system "a" and the output
system is "b"

note: uses sqrt() from math library
*****************************************************************
compute_transformation_coef (ax,ay,bx,by,use,n)

    double ax[], ay[];       coordinate from system a
    double bx[], by[];       coordinate from system b
    char   use[];            use point flags
    int n;                   number of points in ax,ay,bx,by

The first step is to compute coefficients for a set of  equations
which  are then used to convert from the one system to the other.
A set of x,y points from both systems is input into the  equation
generator  which  determines the equation coefficients which most
nearly represent the original points. These coefficients are kept
in a static variables internal to this file.

note: use[i] must be true for ax[i],ay[i],bx[i],by[i] to be used
      in the equation

      also, the total number of used points must be 4 or larger

returns:
   -2  less than 4 used points.
   -1  couldn't solve the equation. points probably colinear
       and need to be spread out more.
    1  ok
*****************************************************************
Then points from one system may be converted into the  second  by
use of one of the two equation routines.

transform_a_into_b (ax,ay,bx,by)

    double ax,ay;            input point from system a
    double *bx,*by;          resultant point in system b

transform_b_into_a (bx,by,ax,ay)

    double bx,by;            input point from system b
    double *ax,*ay;          resultant point in system a
*****************************************************************
Residual analysis on the equation can be run to test how well
the equations work.  Either test how well b is predicted by a
or vice versa.

residuals_a_predicts_b (ax,ay,bx,by,use,n,residuals,rms)
residuals_b_predicts_a (ax,ay,bx,by,use,n,residuals,rms)

    double ax[], ay[];       coordinate from system a
    double bx[], by[];       coordinate from system b
    char   use[];            use point flags
    int n;                   number of points in ax,ay,bx,by
    double residual[]        residual error for each point
    double *rms;             overall root mean square error
****************************************************************/

#include <math.h>
#include "libtrans.h"

/* the coefficients */
static double A0,A1,A2,A3,A4,A5;
static double B0,B1,B2,B3,B4,B5;

static int resid(
    double *,double *,double *,double *,int *,int,double *,double *,int);

int compute_transformation_coef(
    double ax[],double ay[],double bx[],double by[],
    int use[],int n)
{
    int i;
    int j;
    int count;
    double aa[3];
    double aar[3];
    double bb[3];
    double bbr[3];

    double cc[3][3];
    double x;

    count = 0;
    for (i = 0; i < n; i++)
	if (use[i])
	    count++;
    if (count < 4)
	return -2; /* must have at least 4 points */

    for (i = 0; i < 3; i++)
    {
        aa[i] = bb[i] = 0.0;

        for (j = 0; j < 3; j++)
	    cc[i][j] = 0.0;
    }

    for (i = 0; i < n; i++)
    {
	if (!use[i]) continue;	/* skip this point */
        cc[0][0] += 1;
        cc[0][1] += bx[i];
        cc[0][2] += by[i];

        cc[1][1] += bx[i] * bx[i];
        cc[1][2] += bx[i] * by[i];
        cc[2][2] += by[i] * by[i];

        aa[0] += ay[i];
        aa[1] += ay[i] * bx[i];
        aa[2] += ay[i] * by[i];

        bb[0] += ax[i];
        bb[1] += ax[i] * bx[i];
        bb[2] += ax[i] * by[i];
    }

    cc[1][0] = cc[0][1];
    cc[2][0] = cc[0][2];
    cc[2][1] = cc[1][2];

/* aa and bb are solved */
	
	if ( inverse (cc) < 0)
		return (-1) ;
	if ( m_mult ( cc, aa, aar) < 0  ||  m_mult ( cc, bb, bbr) < 0)
		return (-1) ;



/* the equation coefficients */

    B0 = aar[0];
    B1 = aar[1];
    B2 = aar[2];

    B3 = bbr[0];
    B4 = bbr[1];
    B5 = bbr[2];

/* the inverse equation */

    x = B2 * B4 - B1 * B5 ;

    if( ! x)
	return (-1) ;

    A0 = (B1 * B3 - B0 * B4) / x ;
    A1 = -B1 / x ;
    A2 =  B4 / x ;
    A3 = (B0 * B5 - B2 * B3) / x ;
    A4 = B2 / x ;
    A5 = -B5 / x ;

    return 1;
}

int transform_a_into_b(
    double ax,double ay,
    double *bx,double *by)
{
    *by = A0 + A1 * ax + A2 * ay ;
    *bx = A3 + A4 * ax + A5 * ay ;

    return 0;
}

int transform_b_into_a(
    double bx,double by,
    double *ax,double *ay)
{
    *ay = B0  + B1 * bx + B2 * by ;
    *ax = B3  + B4 * bx + B5 * by ;

    return 0;
}

/**************************************************************
These routines are internal to this source code

solve (a, b)
    double a[3][3]
    double b[3]

    equation solver used by compute_transformation_coef()
**************************************************************/

/*  #define abs(xx) (xx >= 0 ? xx : -xx)  */
/*	#define N 3  */


int residuals_a_predicts_b (
    double ax[],double ay[],double bx[],double by[],
    int use[],
    int n,
    double residuals[],
    double *rms)
{
    resid (ax,ay,bx,by,use,n,residuals,rms,1);

    return 0;
}

int residuals_b_predicts_a(
    double ax[],double ay[],double bx[],double by[],
    int use[],
    int n,
    double residuals[],
    double *rms)
{
    resid (ax,ay,bx,by,use,n,residuals,rms,0);

    return 0;
}

static int resid(
    double ax[],double ay[],double bx[],double by[],
    int use[],int n,
    double residuals[],
    double *rms,
    int atob)
{
    double x,y;
    int i;
    int count;
    double sum;
    double delta;
    double dx,dy;

    count = 0;
    sum = 0.0;
    for (i=0; i < n; i++)
    {
	if (!use[i]) continue;

	count++;
	if (atob)
	{
	    transform_a_into_b (ax[i], ay[i], &x, &y);
	    dx = x - bx[i];
	    dy = y - by[i];
	}
	else
	{
	    transform_b_into_a (bx[i], by[i], &x, &y);
	    dx = x - ax[i];
	    dy = y - ay[i];
	}

	delta = dx * dx + dy * dy;
	residuals[i] = sqrt (delta);
	sum += delta;
    }
    *rms = sqrt (sum / count);

    return 0;
}
