/*
* $Id$
*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Original author CERL, probably Dave Gerdes.
*               Update to GRASS 5.1 Radim Blazek.
*
* PURPOSE:      Lower level functions for reading/writing/manipulating vectors.
*
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include <math.h>
#include "Vect.h"


#ifndef HUGE_VAL
#define HUGE_VAL 9999999999999.0
#endif

/*
**  fills BPoints (must be inited previously) by points from imput
**  array LPoints. Each imput points must have at least 2 points.
**   
**  returns number of points or -1 on error
*/


int 
dig_get_poly_points ( int n_lines,
		      struct line_pnts **LPoints,
		      int *direction,             /* line direction: > 0 or < 0 */
		      struct line_pnts *BPoints
		      )
{
    register int i, j, point, start, end, inc;
    struct line_pnts *Points;
    int n_points;

    BPoints->n_points = 0;

    if ( n_lines < 1 ) { return 0; } 
  
    /* Calc required space */
    n_points = 0;
    for (i = 0; i < n_lines; i++) {
        Points = LPoints[i];
        n_points += Points->n_points - 1; /* each line from first to last - 1 */
    }
    n_points++; /* last point */ 
      
    if (0 > dig_alloc_points (BPoints, n_points))
	return (-1);

    point = 0; j = 0;
    for (i = 0; i < n_lines; i++) {
        Points = LPoints[i];
        if (direction[i] > 0) {
	  start = 0;
	  end   = Points->n_points - 1;
	  inc   = 1;
	} else {
	  start = Points->n_points - 1;
	  end   = 0;
	  inc   = -1;
	}

        for ( j = start; j != end; j += inc) {
	  BPoints->x[point] = Points->x[j];
	  BPoints->y[point] = Points->y[j];
	}
	point++;
    }
    /* last point */
    BPoints->x[point] = Points->x[j];
    BPoints->y[point] = Points->y[j];
    
    BPoints->n_points = n_points;

    return (BPoints->n_points);
}

/*
   ** same as dig_point_in_area () , execpt that it works for 
   ** a generic polygon, build with 'Points'
   **   dpg 12/89  (for Vcontour or whatever it may be called by the time
   **               this is read)
   **
   **   WARNING:  if poly is an area,  this will NOT tell you if it inside
   **     an Island w/in the area.
 */
double
dig_point_in_poly (
		    double X, double Y,
		    struct line_pnts *Points)
{
  double *x, *y;
  double cur_min;
  double cur_x, cur_y;
  double dig_x_intersect ();
  double x_inter;
  double x_dist;
  int n_intersects;
  int n_segs;
  int n;

  cur_min = HUGE_VAL;
  cur_x = 0.0;
  cur_y = 0.0;
  n_intersects = 0;

/* adjust yarray coordinates */
  y = Points->y;
  for (n = 0; n < Points->n_points; n++)
    {
      if (*y == Y)
	*y = Y * 1.000000001;	/* TODO actually changing data */
      y++;
    }

/* Point loop */
  x = Points->x;
  y = Points->y;
  cur_x = *x;
  x++;
  cur_y = *y;
  y++;
  n_segs = Points->n_points - 1;

  for (n = 0; n < n_segs; n++)
    {
      if ((cur_y < Y && *y < Y)
	  || (cur_y > Y && *y > Y)
	  || (cur_x < X && *x < X))
	{
	  cur_x = *x;
	  x++;
	  cur_y = *y;
	  y++;
	  continue;
	}

      x_inter = dig_x_intersect (cur_x, *x, cur_y, *y, Y);
      if (x_inter > X)
	{
	  n_intersects++;

	  x_dist = fabs (x_inter - X);
	  if (x_dist < cur_min)
	    cur_min = x_dist;
	}

      cur_x = *x;
      x++;
      cur_y = *y;
      y++;
    }

  if (n_intersects % 2)
    return (cur_min);
  else
    return (0.0);
}

/*
**  Calculate area size for polygon. 
**
**  Total area is positive for clockwise and negative for counter clockwise ?
*/
int 
dig_find_area_poly (
    struct line_pnts *Points,
    double *totalarea)
{
  int i;
  double *x, *y;
  double tot_area, sum_area;


  *totalarea = 0.;

  tot_area = 0.0;

  x = Points->x;
  y = Points->y;

  sum_area = 0.0;
  for (i = 1; i < Points->n_points; i++)
    {
      sum_area += (x[i] - x[i - 1]) * (y[i] + y[i - 1]);
    }
  tot_area += sum_area;

  *totalarea = 0.5 * tot_area;

  return (0);
}
