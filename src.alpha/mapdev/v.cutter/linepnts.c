/**** linepnts.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "cutter.h"


/* return the next vertex point after the specified 'pnum' for real line 'lnum',
**  with respect to area 'anum'.   if dir == REVERSE, then the points in `
**  'Points' are backwards wrt 'anum'.
**
**  Note anum can be negative for island
*/

/*
** NOTE uses the TPoints structure which is global to this program
*/  

struct point_t *
next_vert (Map, Points, lnum, pnum, anum, dir)	      /* ==ISLE== */
    struct Map_info *Map;
    struct line_pnts *Points;
    plus_t lnum;	/* line number in map */
    int    pnum;	/* point number in line */
    plus_t anum;	/* area number */
    int dir;
{
  static struct point_t Point;
  plus_t line;
  int ret;

  if (dir == REVERSE)
  {
      if (pnum > 0)	/* just return prev point */
      {
	pnum--;
	Point.x = Points->x[pnum];
	Point.y = Points->y[pnum];
	return &Point;
      }
  }
  else
  {
      if (pnum < Points->n_points -1)	/* just return next point */
      {
	pnum++;
	Point.x = Points->x[pnum];
	Point.y = Points->y[pnum];
	return &Point;
      }
  }
  /* else have to look to next line in area */

  line = get_next_area_line (Map, anum, lnum);			/* =ISLE= */
  ret = V2_read_line_poly_order (Map, TPoints, line, anum); 	/* =ISLE= */
  if (ret == -2 || ret == 0)	/* not really accurate */
  {
    dig_out_of_memory ();
  }
  /*
  **  Note that point[0] is same as last point of prev line so 
  **   give next point 
  */
  Point.x = TPoints->x[1];
  Point.y = TPoints->y[1];
  return &Point;
}

#ifdef FOO
/*
** TODO not up to date
*/
struct point_t *
prev_vert (Map, Points, lnum, pnum, anum, dir)
    struct Map_info *Map;
    struct line_pnts *Points;
    plus_t lnum;	/* line number in map */
    int    pnum;	/* point number in map */
    plus_t anum;	/* area number */
    int dir;
{
  static struct point_t Point;
  plus_t line;
  int ret;

  if (dir == REVERSE)
  {
    if (pnum < Points->n_points -1)	/* just return next point */
    {
      pnum++;
      Point.x = Points->x[pnum];
      Point.y = Points->y[pnum];
      return &Point;
    }
  }
  else
  {
     if (pnum > 0)	/* just return prev point */
     {
       pnum--;
       Point.x = Points->x[pnum];
       Point.y = Points->y[pnum];
       return &Point;
     }
  }
  /* else have to look to prev line in area */

  line = get_prev_area_line (Map, anum, lnum);
  ret = V2_read_line_poly_order (Map, TPoints, line, anum);
  if (ret == -2 || ret == 0)	/* not really accurate */
  {
/*DEBUG*/ fprintf (stderr, "2>>>>>>>>>>>>>>>  ");
    dig_out_of_memory ();
  }
  /*
  **  Note that point[END] is same as first point of next line so 
  **   give prev point 
  */
  Point.x = TPoints->x[TPoints->n_points-2];
  Point.y = TPoints->y[TPoints->n_points-2];
  return &Point;
}
#endif


/*
** return 1 if segments are co-linear    (really EQUAL )
**  else 0
*/
seg_colinear (A1, A2, B1, B2)
  struct point_t *A1, *A2, *B1, *B2;
{
  return 
    (B1->x == A2->x && B2->x == A1->x) && (B1->y == A2->y && B2->y == A1->y)  ||
    (B1->x == A1->x && B2->x == A2->x) && (B1->y == A1->y && B2->y == A2->y) ;
}
