/*
* $Id$
*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Original author CERL, probably Dave Gerdes or Mike Higgins.
*               Update to GRASS 5.1 Radim Blazek and David D. Gray.
*
* PURPOSE:      Higher level functions for reading/writing/manipulating vectors.
*
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include <stdlib.h>
#include "gis.h"
#include "Vect.h"

/*
   **  returns the polygon array of points  in BPoints
   **   returns  number of points or -1 on error
 */


int 
Vect_get_area_points (
		       struct Map_info *Map,
		       int area,
		       struct line_pnts *BPoints)
{
  int i, line, aline, dir;
  struct Plus_head *Plus;
  P_AREA *Area;
  static int first_time = 1;
  static struct line_pnts *Points;
  
  G_debug ( 3, "Vect_get_area_points(): area = %d", area );	
  BPoints->n_points = 0;

  Plus = &(Map->plus);
  Area = Plus->Area[area];

  if (first_time == 1){
      Points = Vect_new_line_struct ();	
      first_time = 0;
  }

  G_debug ( 3, "  n_lines = %d", Area->n_lines );	
  for (i = 0; i < Area->n_lines; i++)
    {
      line = Area->lines[i];
      aline = abs (line);
      G_debug ( 3, "  append line(%d) = %d", i, line );	

      if (0 > Vect_read_line (Map, Points, NULL, aline)) {
          G_fatal_error ( "Cannot read line %d",  aline );
      }
      
      G_debug ( 3, "  line n_points = %d", Points->n_points );	

      if ( line > 0 )
          dir = GV_FORWARD;
      else 
	  dir = GV_BACKWORD;

      Vect_append_points ( BPoints, Points, dir);  
      if ( i != (Area->n_lines - 1) ) /* all but not last */
	 BPoints->n_points--; 
      G_debug ( 3, "  area n_points = %d", BPoints->n_points );	
    }

  return (BPoints->n_points);
}

int 
Vect_get_isle_points (
		       struct Map_info *Map,
		       int isle,
		       struct line_pnts *BPoints)
{
  int i, line, aline, dir;
  struct Plus_head *Plus;
  P_ISLE *Isle;
  static int first_time = 1;
  static struct line_pnts *Points;
  
  G_debug ( 3, "Vect_get_isle_points(): isle = %d", isle );	
  BPoints->n_points = 0;

  Plus = &(Map->plus);
  Isle = Plus->Isle[isle];

  if (first_time == 1) {
      Points = Vect_new_line_struct ();	
      first_time = 0;
  }

  G_debug ( 3, "  n_lines = %d", Isle->n_lines );	
  for (i = 0; i < Isle->n_lines; i++)
    {
      line = Isle->lines[i];
      aline = abs (line);
      G_debug ( 3, "  append line(%d) = %d", i, line );	

      if (0 > Vect_read_line (Map, Points, NULL, aline)) {
          G_fatal_error ( "Cannot read line %d",  aline );
      }
      
      G_debug ( 3, "  line n_points = %d", Points->n_points );	

      if ( line > 0 )
          dir = GV_FORWARD;
      else 
	  dir = GV_BACKWORD;

      Vect_append_points ( BPoints, Points, dir);  
      if ( i != (Isle->n_lines - 1) ) /* all but not last */
	 BPoints->n_points--; 
      G_debug ( 3, "  area n_points = %d", BPoints->n_points );	
    }

  return (BPoints->n_points);
}

/* Returns centroid number or 0 */ 
int 
Vect_get_area_centroid (
		       struct Map_info *Map,
		       int area )
{
  struct Plus_head *Plus;
  P_AREA *Area;
  
  G_debug ( 3, "Vect_get_area_centroid(): area = %d", area );	

  Plus = &(Map->plus);
  Area = Plus->Area[area];
  return ( Area->centroid );
}

/* Returns number of isles */ 
int 
Vect_get_area_num_isles (
		       struct Map_info *Map,
		       int area )
{
  struct Plus_head *Plus;
  P_AREA *Area;
  
  G_debug ( 3, "Vect_get_area_num_isles(): area = %d", area );	

  Plus = &(Map->plus);
  Area = Plus->Area[area];
  return ( Area->n_isles );

}

/* Returns area isle */ 
int 
Vect_get_area_isle (
		       struct Map_info *Map,
		       int area,
                       int isle)
{
  struct Plus_head *Plus;
  P_AREA *Area;
  
  G_debug ( 3, "Vect_get_area_isle(): area = %d", area );	

  Plus = &(Map->plus);
  Area = Plus->Area[area];
  return ( Area->isles[isle] );
}

/* 
*  returns 1 if point is in area
*          0 if not
*/

int 
Vect_point_in_area (
		       struct Map_info *Map,
		       int area,
		       double x, double y)
{
  int    i, isle;
  struct Plus_head *Plus;
  P_AREA *Area;
  double poly;
  static struct line_pnts *Points;
  static int first_time = 1;
  
  Plus = &(Map->plus);
  Area = Plus->Area[area];
  if ( Area == NULL ) return 0;

  if (first_time == 1)
    {
      Points = Vect_new_line_struct ();	
      first_time = 0;
    }
  
  Vect_get_area_points (Map, area, Points);
  poly = dig_point_in_poly ( x, y, Points);
  if (poly <= 0) return 0;


  /* check if in islands */
  for (i = 0; i < Area->n_isles; i++) {
      isle = Area->isles[i];
      Vect_get_isle_points (Map, isle, Points);
      poly = dig_point_in_poly ( x, y, Points);
      if (poly > 0) return 0;
  }

  return 1;
}

/* Returns area of are without areas of isles */ 
double 
Vect_get_area_area (
		       struct Map_info *Map,
		       int area)
{
  struct Plus_head *Plus;
  P_AREA *Area;
  struct line_pnts * Points;
  double size;
  int i;
  
  G_debug ( 3, "Vect_get_area_area(): area = %d", area );	

  Points = Vect_new_line_struct();
  Plus = &(Map->plus);
  Area = Plus->Area[area];
 
  Vect_get_area_points(Map, area, Points);
  size = G_area_of_polygon(Points->x, Points->y, Points->n_points);

  /* substructing island areas */
  for(i = 0; i < Area->n_isles; i++) {
      Vect_get_isle_points(Map, Area->isles[i], Points);
      size -= G_area_of_polygon(Points->x, Points->y, Points->n_points);
  }
  
  Vect_destroy_line_struct(Points);
  
  return ( size );
}

