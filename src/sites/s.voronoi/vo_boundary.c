/* This modules handles lines that form the region bound to ensure that
   they don't cross any line of the Voronoi diagram. Four array of coordinates
   are allocated, one for each bound, and each one keeps the intersection coordinates
   between the region rectangle bound and the Voronoi diagram. When all intersection
   points are collected coordinates are sorted and then dumped to build to rectangle
   by segments that don't cross Voronoi diagram lines. This ensures that the output
   file is ready for v.support so that v.spag is not needed to build a topology

   History: Aime Andrea 22/4/2001: writed from scratch
*/

#include <stdlib.h>
#include "gis.h"
#include "Vect.h"
#include "vo_defs.h"

#define ALLOC_INT 50

double *northCross, *southCross,      /* coordinates of points in which voronoi diagram */
       *eastCross, *westCross;        /* intersects region boundary                     */
int nci, sci, eci, wci;               /* boundary intersection array indices            */
int nca, sca, eca, wca;               /* amount allocated                               */
double north, south, east, west;


/* prepare boundary arrays, indices and insert extreme values */
void prepareBoundaryArrays(struct Cell_head w)
{
    north = w.north;
    south = w.south;
    east = w.east;
    west = w.west;
    nca = sca = wca = eca = ALLOC_INT;
    northCross = (double *) G_malloc(nca * sizeof(double));
    southCross = (double *) G_malloc(sca * sizeof(double));
    eastCross = (double *) G_malloc(eca * sizeof(double));
    westCross = (double *) G_malloc(wca * sizeof(double));
    northCross[0] = southCross[0] = east;
    northCross[1] = southCross[1] = west;
    eastCross[0] = westCross[0] = south;
    eastCross[1] = westCross[1] = north;
    nci = sci = wci = eci = 2;
}


/* add a point into a boundary array allocating more memory if needed */
void addBoundaryPoint(double coor, double **vect, int *index, int *alloc)
{
    if((*index) >= (*alloc)) {
        *alloc = (*alloc) + ALLOC_INT;
        (*vect) = (double *) G_realloc((*vect), (*alloc) * sizeof(double));
    }

    (*vect)[*index] = coor;
    (*index)++;
}


/* chech for boundary intersection and add points */
void handleBoundaryPoint(double x, double y)
{
    if(x == east)
        addBoundaryPoint(y, &eastCross, &eci, &eca);
    else if(x == west)
        addBoundaryPoint(y, &westCross, &wci, &wca);
    else if(y == south)
        addBoundaryPoint(x, &southCross, &sci, &sca);
    else if(y == north)
        addBoundaryPoint(x, &northCross, &nci, &nca);
}


int compDouble(const void *v1, const void *v2)
{
    double *d1, *d2;
    d1 = (double *) v1;
    d2 = (double *) v2;
    if((*d1) == (*d2)) return 0;
    return ((*d1) < (*d2)) ? -1: 1;
}


void sortedBoundary(double *vect, int count, double **sb, int *sbCount)
{
    int i, j;

    /*
    fprintf(stderr, "\n(");
    for(i = 0; i < count; i++)
       fprintf(stderr, "%f \t", vect[i]);
    fprintf(stderr, ")\n");
    */

    /* sort original vector and try a first allocation of output array */
    qsort(vect, count, sizeof(double), compDouble);
    (*sb) = (double *) G_malloc(count * sizeof(double));

    /* uncomment to get some debug output
    fprintf(stderr, "\n(");
    for(i = 0; i < count; i++)
       fprintf(stderr, "%f \t", vect[i]);
    fprintf(stderr, ")\n"); */

    /* copy without duplicates */
    (*sb)[0] = vect[0];
    j = 1;
    for(i = 1; i < count; i++)
        if(vect[i] != vect[i - 1])
        {
            (*sb)[j] = vect[i];
            j++;
        }

    /* adjust dimension */
    if(j < count)
        (*sb) = (double *) G_realloc((*sb), j * sizeof(double));
    *sbCount = j;
}


/* output a boundary. Having stored crossing coordinates now the program
   output each line segments composing the boundary in order to build a
   proper topology without the need to run v.spag. Duplicate coordinates
   elimination is necessary to avoid spurious segments that may confuse
   v.support
*/
void outputBoundary(int boundary, struct Map_info *Map, struct line_pnts *Points)
{
    int count, i;           /* number of coordinates and index */
    double *tmpx, *tmpy;    /* coordinates array */
    double *xcur, *ycur;    /* pointer indices */

    /* sort, eliminate duplicates and realloc */
    switch(boundary)
    {
        case NORTH:
            sortedBoundary(northCross, nci, &tmpx, &count);
            tmpy = (double *) G_malloc(count * sizeof(double));
            for(i = 0; i < count; i++)
                tmpy[i] = north;
            break;
        case SOUTH:
            sortedBoundary(southCross, sci, &tmpx, &count);
            tmpy = (double *) G_malloc(count * sizeof(double));
            for(i = 0; i < count; i++)
                tmpy[i] = south;
            break;
        case EAST:
            sortedBoundary(eastCross, eci, &tmpy, &count);
            tmpx = (double *) G_malloc(count * sizeof(double));
            for(i = 0; i < count; i++)
                tmpx[i] = east;
            break;
        case WEST:
            sortedBoundary(westCross, wci, &tmpy, &count);
            tmpx = (double *) G_malloc(count * sizeof(double));
            for(i = 0; i < count; i++)
                tmpx[i] = west;
            break;
        default:
            return;
    }

    /* output values couple by couple */
    for(xcur = tmpx, ycur = tmpy, i = 0; i < count - 1; i++, xcur++, ycur++)
    {
        if (0 > Vect_copy_xy_to_pnts (Points, xcur, ycur, 2))
            G_fatal_error ("Out of memory");
        Vect_write_line (Map, AREA, Points);
    }

    G_free(tmpx);
    G_free(tmpy);
}


void freeBoundaryArrays()
{
    G_free(northCross);
    G_free(southCross);
    G_free(eastCross);
    G_free(westCross);
}
