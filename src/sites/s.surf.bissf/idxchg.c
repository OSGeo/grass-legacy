#include<stdio.h>
#include<math.h>

#define abs(x) ((x) >= 0 ? (x) : -(x))
#define dabs(x) (double)abs(x)
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))
#define dmin(a,b) (double)min(a,b)
#define dmax(a,b) (double)max(a,b)
 
int idxchg_(x, y, i1, i2, i3, i4)
float *x, *y;
int *i1, *i2, *i3, *i4;
{
    /* System generated locals */
    int ret_val;
    float r__1, r__2;
    static float equiv_0[1], equiv_1[1], equiv_2[1], equiv_3[1], equiv_4[1], 
	    equiv_5[1];

    /* Local variables */
    static float u1, u2, u3, x1, y1, x2, y2, x3, y3, x4, y4, u4;
    static int idx;
#define a1sq (equiv_2)
#define b1sq (equiv_3)
#define c1sq (equiv_0)
#define c2sq (equiv_0)
#define a3sq (equiv_1)
#define b2sq (equiv_1)
#define b3sq (equiv_2)
#define a4sq (equiv_3)
#define b4sq (equiv_4)
#define a2sq (equiv_4)
#define c4sq (equiv_5)
#define c3sq (equiv_5)
    static float s1sq, s2sq, s3sq, s4sq;

/* THIS FUNCTION DETERMINES WHETHER OR NOT THE EXCHANGE OF TWO */
/* TRIANGLES IS NECESSARY ON THE BASIS OF MAX-MIN-ANGLE CRITERION */
/* BY C. L. LAWSON. */
/* THE INPUT PARAMETERS ARE */
/*     X,Y = ARRAYS CONTAINING THE COORDINATES OF THE DATA */
/*           POINTS, */
/*     I1,I2,I3,I4 = POINT NUMBERS OF FOUR POINTS P1, P2, */
/*           P3, AND P4 THAT FORM A QUADRILATERAL WITH P3 */
/*           AND P4 CONNECTED DIAGONALLY. */
/* THIS FUNCTION RETURNS AN INTEGER VALUE 1 (ONE) WHEN AN EX- */
/* CHANGE IS NECESSARY, AND 0 (ZERO) OTHERWISE. */
/* DECLARATION STATEMENTS */
/* PRELIMINARY PROCESSING */
    /* Parameter adjustments */
    --y;
    --x;

    /* Function Body */
/* L10: */
    x1 = x[*i1];
    y1 = y[*i1];
    x2 = x[*i2];
    y2 = y[*i2];
    x3 = x[*i3];
    y3 = y[*i3];
    x4 = x[*i4];
    y4 = y[*i4];
/* CALCULATION */
/* L20: */
    idx = 0;
    u3 = (y2 - y3) * (x1 - x3) - (x2 - x3) * (y1 - y3);
    u4 = (y1 - y4) * (x2 - x4) - (x1 - x4) * (y2 - y4);
    if (u3 * u4 <= (float)0.) {
	goto L30;
    }
    u1 = (y3 - y1) * (x4 - x1) - (x3 - x1) * (y4 - y1);
    u2 = (y4 - y2) * (x3 - x2) - (x4 - x2) * (y3 - y2);
/* Computing 2nd power */
    r__1 = x1 - x3;
/* Computing 2nd power */
    r__2 = y1 - y3;
    *a1sq = r__1 * r__1 + r__2 * r__2;
/* Computing 2nd power */
    r__1 = x4 - x1;
/* Computing 2nd power */
    r__2 = y4 - y1;
    *b1sq = r__1 * r__1 + r__2 * r__2;
/* Computing 2nd power */
    r__1 = x3 - x4;
/* Computing 2nd power */
    r__2 = y3 - y4;
    *c1sq = r__1 * r__1 + r__2 * r__2;
/* Computing 2nd power */
    r__1 = x2 - x4;
/* Computing 2nd power */
    r__2 = y2 - y4;
    *a2sq = r__1 * r__1 + r__2 * r__2;
/* Computing 2nd power */
    r__1 = x3 - x2;
/* Computing 2nd power */
    r__2 = y3 - y2;
    *b2sq = r__1 * r__1 + r__2 * r__2;
/* Computing 2nd power */
    r__1 = x2 - x1;
/* Computing 2nd power */
    r__2 = y2 - y1;
    *c3sq = r__1 * r__1 + r__2 * r__2;
    s1sq = u1 * u1 / (*c1sq * dmax(*a1sq,*b1sq));
    s2sq = u2 * u2 / (*c2sq * dmax(*a2sq,*b2sq));
    s3sq = u3 * u3 / (*c3sq * dmax(*a3sq,*b3sq));
    s4sq = u4 * u4 / (*c4sq * dmax(*a4sq,*b4sq));
    if (dmin(s1sq,s2sq) < dmin(s3sq,s4sq)) {
	idx = 1;
    }
L30:
    ret_val = idx;
    return ret_val;
} /* idxchg_ */
