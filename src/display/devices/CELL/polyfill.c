#include <stdio.h>
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

static POINT *P;
static int np;
static int npmax;

polyfill (x,y,n,fill)

    int *x; /* x coordinates of vertices */
    int *y; /* y coordinates of vertices */
    int n;      /* number of verticies */
    int (*fill)();  /* fill routine */
{
    int edge_order();
    char *calloc();
    int x0,x1,xN;
    int y0,y1,yN;
    int error;

    if (n < 3)
        return 0;

/* traverse the perimeter */

    np = 0;
    P = (POINT *) calloc (npmax = 32, sizeof (POINT));

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

    register int x0, y0 ;
    int x1, y1 ;
{
    register float m;
    register float x;


    if (y0 == y1) return;

    m = (float) (x0 - x1) / (float) (y0 - y1) ;

    if (y0 < y1)
    {
        x = x0;
        while (++y0 <= y1)
        {
            x0 = (x += m) + .5;
            edge_point (x0, y0);
        }
    }
    else
    {
        x = x1;
        while (++y1 <= y0)
        {
            x1 = (x += m) + .5;
            edge_point (x1, y1);
        }
    }
}

static
edge_point (x, y)
    register int x, y;
{
    char *realloc();

    if (np >= npmax)
        P = (POINT *) realloc (P, (npmax += 32) * sizeof (POINT));
    P[np].x   = x;
    P[np++].y = y;
}

static
edge_order (a, b)

    struct point *a, *b;
{
    if (a->y < b->y) return (-1);
    if (a->y > b->y) return (1);

    if (a->x < b->x) return (-1);
    if (a->x > b->x) return (1);

    return (0);
}
