#include <math.h>
#include "Vect.h"


#ifndef HUGE_VAL
#define HUGE_VAL 9999999999999.0
#endif

/* original dig_point_to_line() in grass50
* 
* 
* type = LINE AREA or POINT  if only want to search certain types of lines
*         or -1 if search all lines
* maxdist = max distance from the line
*
* returns : nearest line
*           0 not found
*/
int 
Vect_find_line (
		    struct Map_info *map,
		    double ux, double uy,
		    int type, double maxdist )
{
  int choice;
  double new_dist;
  double cur_dist;
  int gotone;
  int i;
  static struct line_pnts *Points;
  static int first_time = 1;
  struct Plus_head *Plus;
  P_LINE *Line;
  
  if (first_time) {
      Points = Vect_new_line_struct ();
      first_time = 0;
  }
  
  Plus = &(map->plus);
  gotone = 0;
  choice = 0;
  cur_dist = HUGE_VAL;

  for (i = 1; i <= Plus->n_lines; i++) {
      Line = Plus->Line[i]; 	
      if ( Line == NULL ) continue;
      
      /* limit searches to specific line types */
      if ( !(type & Line->type) ) continue;

      /* TODO use bbox */
  
      V2_read_line (map, Points, NULL, i);

      Vect_line_distance ( Points, ux, uy, NULL, NULL, &new_dist, NULL, NULL);
      G_debug( 3, " line = %d distance = %f", i,  new_dist);
      if ((++gotone == 1) || (new_dist <= cur_dist)) {
	  if (new_dist == cur_dist)
	    {
	      /* TODO */  
	      //choice = dig_center_check (map->Line, choice, a, ux, uy);
	      continue;
	    }
	  
	  choice = i;
	  cur_dist = new_dist;
       }
  }
  
  G_debug( 3, "min distance found = %f, maxdist = %f", cur_dist, maxdist);
  if (cur_dist > maxdist)
      choice = 0;

  return (choice);
}

/* original dig_point_to_area() in grass50
* 
* maxdist = max distance from the line
*
* returns : area number
*           0 not found
*/
int 
Vect_find_area (
		    struct Map_info *map,
		    double x, double y)
{
  int i, ret;
  struct Plus_head *Plus;
  
  Plus = &(map->plus);
  for (i = 1; i <= Plus->n_areas; i++) {
      ret = Vect_point_in_area (map, i, x, y);

      if ( ret )
	  return (i);
  }
  
  return 0;
}

