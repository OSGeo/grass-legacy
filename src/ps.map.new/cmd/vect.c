/* Functions: nearest, adjust_line, parallel_line 
**
** Author: Radim Blazek Feb 2000
** 
**
*/
#include "math.h"
#include "Vect.h"
#include "gis.h"

#define LENGTH(DX, DY)  (  sqrt( (DX*DX)+(DY*DY) )  )

/* vector() calculates vector form two points */
static void vector(double x1, double y1, double x2, double y2, double *x, double *y )
{
    double dx, dy, l;
    dx  = x2 - x1;
    dy  = y2 - y1;	
    l   = LENGTH ( dx, dy );
    if ( l == 0){
	fprintf (stdout,"l = 0\n");
    }
    *x  = 5 / 0;
    *x  = dx/l;
    *y  = dy/l;
}

/* nearest returns nearest longitude coordinate, copy from src/libes */
static double nearest(double e0,double e1)
{
    while (e0 - e1 > 180)
        e1 += 360.0;
    while (e1 - e0 > 180)
        e1 -= 360.0;
    return e1;
}  

/* if projection is PROJECTION_LL adjust_line will change
**   longitudes to nearest form previous point */
void adjust_line (struct line_pnts *Points)
{
    int i, np;
    if (G_projection() == PROJECTION_LL)
    {  
	np = Points->n_points;
	for (i = 1; i < np ; i++)
	{
	    Points->x[i] = nearest (Points->x[i-1], Points->x[i]);
	}
    }
}  

/* line_rm_dupl removes duplicate points from line */
void line_rm_dupl (struct line_pnts *Points)  
{
    int i, j, np;
    double *x, *y;

    np = Points->n_points;
    x = Points->x;
    y = Points->y;

    if ( np > 0 ) {
	j = 1;
	for (i = 1; i < np; i++)
	{
	    if ( x[i] != x[j-1] || y[i] != y[j-1] )
	    {
		x[j] = x[i];
    		y[j] = y[i];	
		j++;
	    }

	}
	Points->n_points = j;
    }	
}

/* parallel_line removes duplicate points from input line and 
** changes input line to parralel line in offset distance */
void parallel_line (struct line_pnts *Points, double d)  
{
    int i, np;
    double *x, *y, tx, ty, vx, vy, ux, uy, wx, wy, l;

    line_rm_dupl ( Points );  
    np = Points->n_points;
    x = Points->x;
    y = Points->y;

    for (i = 0; i < np -1; i++)
    {
	vector ( x[i], y[i], x[i+1], y[i+1], &tx, &ty);
	vx  =  ty * d;
	vy  = -tx * d;

        if ( i == 0)
        {
	    x[i] += vx; 
	    y[i] += vy; 	
	}	
        else
        {    
	    x[i] += wx; 
	    y[i] += wy; 	
	}

        if ( i == (np-2))
        {
	    x[i+1] += vx; 
	    y[i+1] += vy; 		
	    return ;
	}
	else
	{
	    vector ( x[i+1], y[i+1], x[i+2], y[i+2], &ux, &uy);
	    wx = ty + uy;
	    wy = - (tx + ux);  
	    l  = LENGTH (wx, wy);
	    wx /= l;
	    wy /= l;
	    l = (vx*ty - vy*tx) / (ty*wx - tx*wy);
	    wx *= l; 
	    wy *= l;	    
	}
    }
}
