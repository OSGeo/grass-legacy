#include <stdio.h>
#include <stdlib.h>
#include "gis.h"
#include "cell.h"

/********************************************************
polyfill (x, y, n, fill)

   int *x       x coordinates of vertices
   int *y       y coordinates of vertices
   int n        number of verticies
   int (*fill)() user supplied routine to do the filling
                which must be defined as follows:

        fill (row, x1, x2)

        int y, x1, x2

        y is the row
        x1 to x2 is the column range.

        NOTE  ******   fill() must return 0.  *******   NOTE

        if fill() returns non-zero, polyfill will quit and
        return this non-zero value.
********************************************************/

#define POINT struct point
POINT
{
    int x;
    int y;
};

static int edge(int,int,int,int);
static int edge_point(int x,int);
static int edge_order(const void *, const void *);

static POINT *P;
static int np;
static int npmax;

int polyfill (
    int *x, /* x coordinates of vertices */
    int *y, /* y coordinates of vertices */
    int n,      /* number of verticies */
    int (*fill)(int,int,int)  /* fill routine */
)
{
    int error;
    int i;

    if (n < 3)
        return 0;

/* traverse the perimeter */

    np = 0;
    P = (POINT *) G_calloc (npmax = 32, sizeof (POINT));

    for (i = 0; i < n-1; i++)
	edge (x[i],y[i],x[i+1],y[i+1]);
    edge (x[n-1],y[n-1],x[0],y[0]);

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
    for (i = 0; i < np; i += 2)
    {
        if (P[i+1].y != P[i].y)
        {
            fprintf(stderr,"polyfill: row %d out of sync\n", P[i].y);
            continue;
        }
        if(error = fill (P[i].y, P[i].x, P[i+1].x))
            break;
    }

    free (P);

    return error;
}

static int edge ( int x0, int y0, int x1, int y1)
{
    register float m;
    register float x;

    if (y0 == y1) return 0;

    m = (float) (x0 - x1) / (float) (y0 - y1) ;

    if (y0 < y1)
    {
        x = x0;
        while (y0 < y1)
            edge_point (x += m, y0++);
    }
    else
    {
        x = x1;
        while (y1 < y0)
            edge_point (x += m, y1++);
    }

    return 0;
}

static int edge_point ( int x, int y)
{

    if (np >= npmax)
        P = (POINT *) G_realloc (P, (npmax += 32) * sizeof (POINT));
    P[np].x   = x;
    P[np++].y = y;

    return 0;
}

static int edge_order (const void *aa, const void *bb)
{
    const struct point *a = aa, *b = bb;
    if (a->y < b->y) return (-1);
    if (a->y > b->y) return (1);

    if (a->x < b->x) return (-1);
    if (a->x > b->x) return (1);

    return (0);
}
