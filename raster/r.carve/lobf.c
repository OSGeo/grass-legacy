/* 
 * Routines to create a line of best fit when given a set of coord pairs.
 */

#include "enforce.h"
#include <grass/glocale.h>


/*
 * pg_init - initialize PointGrp variables
 */
void pg_init(PointGrp *pg)
{
    pg->sum_x = pg->sum_y = pg->sum_xy = pg->sum_x_sq = 0.0;
    pg->npts = 0;
}


/*
 * pg_y_from_x - determine y value for a given x value in a PointGrp
 */
double pg_y_from_x(PointGrp *pg, double x)
{ 
    return ((pg->slope * x) + pg->yinter); 
}


/*
 * pg_addpt - Add a point to a PointGrp
 */
void pg_addpt(PointGrp *pg, Point2 pt)
{
    if (pg->npts < MAX_PTS - 1)
    {
        double x, y;

        /* add point to group */
        pg->pnts[pg->npts][0] = x = pt[0];
        pg->pnts[pg->npts][1] = y = pt[1];
        pg->sum_x += x;
        pg->sum_y += y;
        pg->sum_xy += (x * y); 
        pg->sum_x_sq += SQR(x);
        ++pg->npts;
    }

    if (pg->npts > 1)
    {
        double denom;

        /* solve for x and y using Cramer's Rule */

        /* check for divide by zero */
        if (0 == (denom = DET2_2(pg->sum_x_sq, pg->sum_x, pg->sum_x, pg->npts)))
        {
            G_warning(_("trying to divide by zero...no unique solution for "
                        "system...skipping..."));
            pg->slope = pg->yinter = 0.0;
        }
        else
        {
            pg->slope  = DET2_2(pg->sum_xy, pg->sum_x, pg->sum_y, pg->npts) / 
                         denom;
            pg->yinter = DET2_2(pg->sum_x_sq, pg->sum_xy, pg->sum_x, pg->sum_y) /
                         denom;
        }
    }
}


/*
 * pg_getpoints - returns the Point2 structure from a PointGrp
 */
Point2 *pg_getpoints(PointGrp *pg)
{
    return pg->pnts;
}


/*
 * pg_getpoints_reversed - reverse points in PointGrp and returns Point2
 */
Point2 *pg_getpoints_reversed(PointGrp *pg)
{
    int i;
    int iter = pg->npts / 2;

    for (i = 0; i < iter; i++) {
        /* swap points */
        pg->pnts[i][0] = pg->pnts[pg->npts-i-1][0];
        pg->pnts[i][1] = pg->pnts[pg->npts-i-1][1];
        pg->pnts[pg->npts-i-1][0] = pg->pnts[i][0];
        pg->pnts[pg->npts-i-1][1] = pg->pnts[i][1];
    }

    /* call recalc? */
    return pg->pnts;
}


#if 0
/* call after doing a pg_getpoints and changing values, 
 * before calling pg_y_from_x again  */
void pg_recalc(PointGrp *pg)
{
    int i;
    double x, y;

    pg->sum_x = pg->sum_y = pg->sum_xy = pg->sum_x_sq = 0.0;

    for (i = 0; i < pg->npts; i++) {
        x = pg->pnts[pg->npts][0];
        y = pg->pnts[pg->npts][1];

        pg->sum_x += x;
        pg->sum_y += y;
        pg->sum_xy += (x * y);
        pg->sum_x_sq += SQR(x);
    }

    if (pg->npts > 1)
    {
        double denom;

        /* solve for x and y using Cramer's Rule */

        /* check for divide by zero */
        if (0 == (denom = DET2_2(pg->sum_x_sq, pg->sum_x, pg->sum_x, pg->npts)))
        {
            G_warning(_("trying to divide by zero...no unique solution for "
                        "system...skipping..."));
            pg->slope = pg->yinter = 0.0;
        }
        else
        {
            pg->slope  = DET2_2(pg->sum_xy, pg->sum_x, pg->sum_y, pg->npts) / 
                         denom;
            pg->yinter = DET2_2(pg->sum_x_sq, pg->sum_xy, pg->sum_x, pg->sum_y) /
                         denom;
        }
    }
}
#endif
