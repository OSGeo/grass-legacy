#include <stdio.h>
#include <math.h>
#include "gis.h"
/********************************************************
polyfill (x, y, n, fill)

   double *x       x coordinates of vertices
   double *y       y coordinates of vertices
   int n        number of verticies
   int (*fill)() user supplied routine to do the filling
                which must be defined as follows:

        fill (y, x1, x2)

        int y;
	double x1, x2;

        y is the row
        x1 to x2 is the column range.

        fill() must return 0.
        if fill() returns non-zero, polyfill will quit and
        return this non-zero value.

Written by Mike Shapiro for integer values
Modified by Dave Gerdes to handle double precision values
********************************************************/

#define POINT struct point
POINT
{
    double x;
    int y;
};

static POINT *P;
static int np;
static int npmax;

polyfill (x,y,n,fill)

    double *x; /* x coordinates of vertices */
    double *y; /* y coordinates of vertices */
    int n;      /* number of verticies */
    int (*fill)();  /* fill routine */
{
    int edge_order();
    double x0,x1,xN;
    double y0,y1,yN;
    int error;
    int i;

    if (n < 3)
        return 0;

/* traverse the perimeter */

    np = 0;
    P = (POINT *) G_calloc (npmax = 100, sizeof (POINT));

    xN = x0 = *x++;
    yN = y0 = *y++;

    while (--n)
    {
        x1 = *x++;
        y1 = *y++;
        edge (x0,y0,x1,y1);
        x0 = x1;
        y0 = y1;
    }
    edge (x0,y0,xN,yN);


/* check if perimeter has odd number of points */
    if (np%2)
    {
        if (P)
            free (P);
        return -99;
    }

/* sort the edge points by row and then by col */


    qsort (P, np, sizeof(POINT), edge_order);

    error = 0;
    for (n = 1; n < np; n++)
    {
        if (P[n].y != P[n-1].y)
        {
            fprintf(stderr,"polyfill: row %d out of sync\n", P[n-1].y);
            continue;
        }
        if(error = fill (P[n].y, P[n-1].x, P[n].x))
            break;
        n++;
    }

    free (P);

    return error;
}

static
edge (x0,y0,x1,y1)

    double x0, y0 ;
    double x1, y1 ;
{
    register double m;
    register double x;
    int ystart, ystop;
    double xstart, xtmp;
    double dy;

    /* tolerance added to avoid FPE */
    if (fabs (dy = y0 - y1)  < 1e-10) 
    {
	return;
    }

    m = (x0 - x1) / (dy) ;
    if (y0 < y1)
    {
	ystart =  iceiling (y0 - .5);
	ystop =  ifloor (y1 - .5);
    }
    else
    {
	ystart =  iceiling (y1 - .5);
	ystop =  ifloor (y0 - .5);
    }

    if (ystart > ystop)	 /* does not cross center line of row */
	return;

    xstart = m * (ystart + .5 - y0) + x0;

    edge_point (xstart, ystart);

    while (++ystart <= ystop)
    {
	xtmp = (xstart += m);
	edge_point (xtmp, ystart);
    }

}


/* edge_point ()  simply stores the selected x/y pair */
static
edge_point (x, y)
    double x;
    register int y;
{
    if (np >= npmax)
    {
        P = (POINT *) G_realloc (P, (npmax += 100) * sizeof (POINT));
    }
    P[np].x   = x;
    P[np++].y = y;
}

/* sort x/y pairs by row and then by col */
/* note that x is a double at this point */
static
edge_order (a, b)
    register struct point *a, *b;
{
    if (a->y < b->y) return (-1);
    if (a->y > b->y) return (1);

    if (a->x < b->x) return (-1);
    if (a->x > b->x) return (1);

    return (0);
}

ifloor (x)
    double x;
{
    return ((int) floor (x));
}

iceiling (x)
    double x;
{
    return ((int) ceil (x));
}
