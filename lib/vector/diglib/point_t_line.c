/*  @(#)p_to_line.c    2.1  6/26/87  */
#include <math.h>
#include "Vect.h"
#include "debugf.h"

/*
   *  functions: point_to_line(), in_line_bbox(), check_dist(), center_check().
   *  point_to_line() loops thru all the lines, calling in_line_bbox() to see
   *  if the line is in the line bounding_box.
   *    then call check_dist() to get distance of the closest point on the line.
   *      if two distances are the same call center_check()  to compare the
   *      two distance's away from the center of the bounding box.
 */

#ifndef HUGE_VAL
#define HUGE_VAL 9999999999999.0
#endif

/*   type == LINE AREA or POINT  if only want to search certain types of lines
   **     or -1 if search all lines
 */
int 
dig_point_to_line (
		    struct Map_info *map,
		    double ux, double uy,
		    char type)
{
  int choice;
  double new_dist;
  double cur_dist;
  int gotone;
  int a;

  gotone = 0;
  choice = 0;
  cur_dist = HUGE_VAL;

  for (a = 1; a <= map->n_lines; a++)
    {
      if (LINE_ALIVE (&(map->Line[a])))
	{
	  /* limit searches to specific line types */
	  if (type & map->Line[a].type)
	    {
	      if (dig_in_line_bbox (&(map->Line[a]), ux, uy))
		{
		  dig_check_dist (map, a, ux, uy, &new_dist);
		  if ((++gotone == 1) || (new_dist <= cur_dist))
		    {
		      if (new_dist == cur_dist)
			{
			  choice = dig_center_check (map->Line, choice, a, ux, uy);
			}
		      else
			{
			  choice = a;
			  cur_dist = new_dist;
			}
		    }
		}
	    }
	}
    }

  return (choice);
}

/*TODO  is not sensitive to scale */
int 
dig_in_line_bbox (
		   P_LINE * line,
		   double x, double y)
{
  if (x < line->W)
    return (0);
  if (x > line->E)
    return (0);
  if (y < line->S)
    return (0);
  if (y > line->N)
    return (0);

  return (1);
}

/* reads neccessary line in from DIGIT */
/* uses its own  line_pnts structure (NOT Gpoints) */
/* to avoid collision */
/* returns the minimum distance squared in dist */
/*  returns the segment number that was the smallest distance */
/*  or -1. on error */
int 
dig_check_dist (
		 struct Map_info *map,
		 int line,
		 double ux,
		 double uy,
		 double *dist)
{
  static int first_time = 1;
  static struct line_pnts points;

  if (first_time)
    {
      points.alloc_points = 0;	/* will be executed once */
      first_time = 0;
    }
  if (0 > V2_read_line (map, &points, NULL, line))
    {
      *dist = HUGE_VAL;
      return (-1);
    }

  return (dig__check_dist (map, &points, ux, uy, dist));
}


int 
dig__check_dist (
		  struct Map_info *map,
		  struct line_pnts *points,
		  double ux,
		  double uy,
		  double *dist)
{
  register int i;
  register double distance;
  register double new_dist;
  register int n_points;
  int segment;


  /*  dpg  2 aug 1989
     corrected this code to work with 1 point lines,  for DOT
     if (points->n_points < 2)
     {
     *dist = HUGE_VAL;
     return (-1);
     }
   */
  n_points = points->n_points;
  segment = 1;

  i = LESSER (n_points - 1, 1);
  distance = dig_distance2_point_to_line (ux, uy, points->x[0], points->y[0],
					  points->x[i], points->y[i]);
  for ( /* i = i */ ; i < n_points - 1; i++)
    {
      new_dist = dig_distance2_point_to_line (ux, uy, points->x[i], points->y[i],
					points->x[i + 1], points->y[i + 1]);
      if (new_dist < distance)
	{
	  distance = new_dist;
	  segment = i + 1;
	}
    }
  *dist = distance;
  return (segment);
}

/*
   ** this code is real hokey, but it only gets called to settle
   ** a dispute when a point is exactly 1/2 way between two lines in question.
   **   i.e. it will probably never get called.
 */
int 
dig_center_check (
		   P_LINE * line,
		   int a, int b,
		   double ux, double uy)
{

  double dist_a;
  double dist_b;

  dist_a = line[a].N - uy;
  dist_b = line[b].N - uy;
  if (dist_a < dist_b)
    return (a);
  if (dist_a > dist_b)
    return (b);

  dist_a = uy - line[a].S;
  dist_b = uy - line[b].S;
  if (dist_a < dist_b)
    return (a);
  if (dist_a > dist_b)
    return (b);

  dist_a = line[a].E - ux;
  dist_b = line[b].E - ux;
  if (dist_a < dist_b)
    return (a);
  if (dist_a > dist_b)
    return (b);

  dist_a = ux - line[a].W;
  dist_b = ux - line[b].W;
  if (dist_a > dist_b)
    return (b);

  return (a);
}

/*
   **  dig_point_BY_line ()
   **
   **   take of on dig_point_to_line () to allow a box of SCREEN pixels
   **   to define inside of BBOX.  This allows for very small bboxes that
   **   are impossible to get with a mouse
 */
int 
dig_point_by_line (
		    struct Map_info *map,
		    double ux1, double uy1,
		    double ux2, double uy2,
		    char type)
{
  int choice;
  double ux, uy;
  double new_dist;
  double cur_dist;
  int gotone;
  int a;

  gotone = 0;
  choice = 0;
  cur_dist = HUGE_VAL;

  ux = (ux1 + ux2) / 2.;	/* get the mid point for nearness test */
  uy = (uy1 + uy2) / 2.;

  for (a = 1; a <= map->n_lines; a++)
    {
      if (LINE_ALIVE (&(map->Line[a])))
	{
	  /* limit searches to specific line types */
	  if (type & map->Line[a].type)
	    {
	      if (dig_by_line_bbox (&(map->Line[a]), ux1, uy1, ux2, uy2))
		{
#ifdef DEBUG
		  debugf ("Entering dig_check_dist\n");
#endif
		  dig_check_dist (map, a, ux, uy, &new_dist);
/*DEBUG */
#ifdef DEBUG
		  debugf ("Leaving dig_check_dist\n");
#endif

		  if ((++gotone == 1) || (new_dist <= cur_dist))
		    {
		      if (new_dist == cur_dist)
			{
#ifdef DEBUG
			  debugf ("Entering dig_center_check\n");
#endif
			  choice = dig_center_check (map->Line, choice, a, ux, uy);
#ifdef DEBUG
			  debugf ("Leaving dig_center_check\n");
#endif
			}
		      else
			{
			  choice = a;
			  cur_dist = new_dist;
			}
		    }
		}
	    }
	}
    }

  return (choice);
}
/*TODO  is not sensitive to scale */

/* 
   **  this compliments dig_point_by_line ()
   **  and is a copy of dig_in_line_bbox ()
   **  X1,X2,Y1,Y2 are corners of a box around the point being
   **  tested.
   **   If this box crosses any lines of the bounding box
   **   or is completely inside the bbxo, it will return true
   **   otherwise, if completely outside the bbox it will return false.
   **
   **
   **    (x1,y1)
   **          *                *
   **                               
   **                               
   **                               
   **                               
   **                               
   **                               
   **          *                *
   **                            (x2,y2)
 */
int 
dig_by_line_bbox (
		   P_LINE * Line,
		   double x1, double y1,
		   double x2, double y2)
{
  if (x1 > Line->E)
    return (0);
  if (x2 < Line->W)
    return (0);
  if (y1 < Line->S)
    return (0);
  if (y2 > Line->N)
    return (0);

  return (1);
}
