/*  @(#)p_to_line.c    2.1  6/26/87  */
#include <math.h>
#include "Vect.h"

/*
   *  functions: point_to_area(),
   *  point_to_area() loops thru all the areas, calling in_area_bbox() to see
   *  if the line is in the area bounding_box. and if it is the closest area
   *  so far.
 */

/* returns area number or 0 if one not found 
 */
int 
dig_point_to_area (
		    struct Map_info *map,
		    double ux, double uy)
{
  register int area;
  int choice;
  double new_dist;
  double cur_dist;
  int gotone;
  /* not static */ int first = 1;
  P_AREA *Area;

  gotone = 0;
  choice = 0;

  for (area = 1; area <= map->n_areas; area++)
    {
      Area = &(map->Area[area]);
      if (AREA_ALIVE (Area))
	{
#ifdef FOO
/*DEBUG */ fprintf (stderr, "checking Area %d (%lf, %lf)\n", area, ux, uy);
#endif
	  if (dig_in_area_bbox (Area, ux, uy))
	    {
	      if (0.0 != (new_dist = dig_point_in_area (map, ux, uy, Area)))
		{
		  if (first)
		    {
		      cur_dist = new_dist;
		      first = 0;
		    }

		  if ((++gotone == 1) || (new_dist <= cur_dist))
		    {
		      /* if (new_dist == cur_dist) ... ; else */
		      {
			choice = area;
			cur_dist = new_dist;
		      }
		    }
		}
	    }
	}
    }

  return (choice);
}

/* 
   ** min_dist returns the distance point is from area.
   ** must be passed a -1.0 to start with.  It can be used to limit
   ** the search area to areas whose minimum distance is farther than min_dist
 */

int 
dig_point_to_next_area (
			 struct Map_info *map,
			 double ux, double uy,
			 double *min_dist)
{
  register int area;
  int choice;
  double new_dist;
  double cur_dist;
  int gotone;
  P_AREA *Area;
  /* not static */ int first = 1;

  gotone = 0;
  choice = 0;

  for (area = 1; area <= map->n_areas; area++)
    {
      Area = &(map->Area[area]);
      if (AREA_ALIVE (Area))
	{
#ifdef DEBUG
	  fprintf (stderr, "pnt_t_n_area: checking Area %d (%lf, %lf)\n", area, ux, uy);
#endif
	  if (dig_in_area_bbox (Area, ux, uy))
	    {
#ifdef DEBUG
	      debugf ("found in BBOX for area %d\n", area);
#endif
	      if (0.0 != (new_dist = dig_point_in_area (map, ux, uy, Area)))
		{
#ifdef DEBUG
		  debugf ("point WAS in area %d\n", area);
#endif
		  if (new_dist > *min_dist)
		    {
		      if (first)
			{
			  cur_dist = new_dist;
			  first = 0;
			}
		      if ((++gotone == 1) || (new_dist <= cur_dist))
			{
			  /* if (new_dist == cur_dist) ... ; else */
			  {
			    choice = area;
			    cur_dist = new_dist;
			  }
			}
		    }
		}
	    }
	}
    }

  *min_dist = cur_dist;
  return (choice);
}
