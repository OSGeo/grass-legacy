/* 
 * Routines to create a line of best fit when given
 * a set of coord pairs.
 *
*/

#include "enforce.h"

/****************************************************************/
pg_init(pg)
PointGrp *pg;
{
  pg->sum_x = pg->sum_y = pg->sum_xy = pg->sum_x_sq = 0.0;
  pg->npts = 0;
}

/****************************************************************/
double pg_y_from_x(pg, x) 
PointGrp *pg;
double x;
{ 
    return ((pg->slope * x) + pg->yinter); 
}

/****************************************************************/
pg_addpt(pg, pt)
PointGrp *pg;
Point2 pt;
{
double x,y,denom;

    if (pg->npts < MAX_PTS - 1){
	pg->pnts[pg->npts][0] = x = pt[0];
	pg->pnts[pg->npts][1] = y = pt[1];
	pg->sum_x += x;
	pg->sum_y += y;
	pg->sum_xy += (x * y); 
	pg->sum_x_sq += (x * x);
	++pg->npts;
    }
    if(pg->npts > 1){
	denom = DET2_2(pg->sum_x_sq, pg->sum_x, pg->sum_x, pg->npts);
	/* should check for 0 denom */
	pg->slope = (DET2_2(pg->sum_xy, pg->sum_x, pg->sum_y, pg->npts)) / 
                     denom;
	pg->yinter = (DET2_2(pg->sum_x_sq, pg->sum_xy, pg->sum_x, pg->sum_y)) /
                     denom;
    }

}

/****************************************************************/
Point2 *pg_getpoints(pg)
PointGrp *pg;
{
    return(pg->pnts);
}

/****************************************************************/
Point2 *pg_getpoints_reversed(pg)
PointGrp *pg;
{
int i, iter;
double bgnx, bgny, endx, endy;

    iter = pg->npts/2;
    for(i=0; i<iter; i++){
	bgnx = pg->pnts[i][0];
	bgny = pg->pnts[i][1];
	endx = pg->pnts[pg->npts-i-1][0];
	endy = pg->pnts[pg->npts-i-1][1];
	pg->pnts[i][0] = endx;
	pg->pnts[i][1] = endy;
	pg->pnts[pg->npts-i-1][0] = bgnx;
	pg->pnts[pg->npts-i-1][1] = bgny;
    }
    /* call recalc? */
    return(pg->pnts);
}

/****************************************************************/
/* call after doing a pg_getpoints and changing values, 
 * before calling pg_y_from_x again 
*/
pg_recalc(pg)
PointGrp *pg;
{
int i;
double x,y,denom;

    pg->sum_x = pg->sum_y = pg->sum_xy = pg->sum_x_sq = 0.0;
    for(i=0; i<pg->npts; i++){
	x = pg->pnts[pg->npts][0];
	y = pg->pnts[pg->npts][1];
	pg->sum_x += x;
	pg->sum_y += y;
	pg->sum_xy += (x * y);
	pg->sum_x_sq += (x * x);
    }
    if(pg->npts > 1){
        denom = DET2_2(pg->sum_x_sq, pg->sum_x, pg->sum_x, pg->npts);
        /* should check for 0 denom */
        pg->slope = (DET2_2(pg->sum_xy, pg->sum_x, pg->sum_y, pg->npts)) /
                     denom;
        pg->yinter = (DET2_2(pg->sum_x_sq, pg->sum_xy, pg->sum_x, pg->sum_y)) /
                     denom;
    }

}
