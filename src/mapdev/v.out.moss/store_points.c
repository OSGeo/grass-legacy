/*
*  COPIES X AND Y COORDINATES INTO DYNAMIC MEMORY POINTED TO BY THE X AND Y
*  FILEDS OF A STRUCTURE OF TYPE LINE_PTS.
*
*  WRITTEN BY:
*  CHRIS EMMERICH, AUTOMETRIC INC., 10/3/89
*/

#include  "Vect.h"

int 
store_points (double *x, double *y, int ncoord, struct line_pnts *points)
{
    int i;

    /* ALLOCATE THE NECESSARY MEMORY IN THE POINTS STRUCTURE */
    if (dig_alloc_points (points,ncoord) < 0) exit(-1);

    /* SET NUMBER OF COORDINATES */
    points->n_points = ncoord;

    /* COPY INTO X AND Y FIELDS */
    for (i = 0; i < ncoord; i++)
    {
        *(points->x+i) = *(x+i);
        *(points->y+i) = *(y+i);
    }

    return (0);
}

