/*
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
#include <math.h>
#include "gis.h"
#include "Vect.h"

#ifndef HUGE_VAL
#define HUGE_VAL 9999999999999.0
#endif

/*!
 \fn int Vect_find_node ( struct Map_info *Map,
		 double ux, double uy, double uz,
		 double maxdist, int with_z )
 \brief find nearest node
 \return number of nearest node, 0 if not found
 \param Map_info structure, ux, uy, uz, maxdist = max distance from the line,
   with_z  - use z coordinate (3D search) 
*/
int 
Vect_find_node ( struct Map_info *Map,
		 double ux, double uy, double uz,
		 double maxdist, int with_z )
{
    int i, nnodes, node;
    BOUND_BOX box;
    struct ilist *NList;
    double x, y, z;
    double cur_dist, dist;

    G_debug ( 3, "Vect_find_node() for %f %f %f maxdist = %f", ux, uy, uz, maxdist);
    NList = Vect_new_list ();
 
    /* Select all nodes in box */
    box.N = uy + maxdist;
    box.S = uy - maxdist;
    box.E = ux + maxdist;
    box.W = ux - maxdist;
    if (with_z) {
        box.T = uz + maxdist;
        box.B = uz - maxdist;
    } else {
        box.T = HUGE_VAL;
        box.B = -HUGE_VAL;
    }
	
    nnodes = Vect_select_nodes_by_box (Map, &box, NList);
    G_debug ( 3, " %d nodes in box", nnodes );

    if ( nnodes == 0 ) return 0;

    /* find nearest */
    cur_dist = PORT_DOUBLE_MAX;
    node = 0;
    for ( i = 0; i < nnodes; i++ ) {
        Vect_get_node_coor ( Map, NList->value[i], &x, &y, &z);
        dist = Vect_points_distance (  ux, uy, uz, x, y, z, with_z ); 
        if ( dist < cur_dist ) {
	    cur_dist = dist;
	    node = i;
	}
    }
    G_debug ( 3, "  nearest node %d in distance %f", NList->value[node], cur_dist );

    /* Check if in max distance */
    if ( cur_dist <= maxdist )
	return ( NList->value[node] );
    else
	return 0;
}

/*!
 \fn int Vect_find_line ( struct Map_info *map,
		 double ux, double uy, double uz,
		 int type, double maxdist, int with_z, int exclude )
 \brief find nearest line
 \return number of nearest line, 0 if not found
 \param Map_info structure, ux, uy, uz, 
    type = GV_LINE, GV_POIN, GV_BOUNDARY or GV_CENTROID if only want to
    search certain types of lines
    or -1 if search all lines,
   maxdist = max distance from the line,
   with_z - use z coordinate (3D search)
*/

/* original dig_point_to_line() in grass50 */
int 
Vect_find_line ( struct Map_info *map,
		 double ux, double uy, double uz,
		 int type, double maxdist, int with_z,
                 int exclude ) /* If > 0 number of line which should be excluded from selection. */
		             /* May be useful if we need line neares to other one. Should be list? */
{
  int choice;
  double new_dist;
  double cur_dist;
  int gotone;
  int i, line;
  static struct line_pnts *Points;
  static int first_time = 1;
  struct Plus_head *Plus;
  struct ilist *List;
  BOUND_BOX box;
  
  G_debug ( 3, "Vect_find_line() for %f %f %f type = %d maxdist = %f exclude = %d", 
	                             ux, uy, uz, type, maxdist, exclude);
    
  if (first_time) {
      Points = Vect_new_line_struct ();
      first_time = 0;
  }
  
  List = Vect_new_list ();
  
  Plus = &(map->plus);
  gotone = 0;
  choice = 0;
  cur_dist = HUGE_VAL;

  box.N = uy + maxdist; box.S = uy - maxdist;
  box.E = ux + maxdist; box.W = ux - maxdist;
  if ( with_z ) {
      box.T = uz + maxdist; box.B = uz - maxdist;
  } else {
      box.T = PORT_DOUBLE_MAX; box.B = -PORT_DOUBLE_MAX;
  }
  
  Vect_select_lines_by_box ( map, &box, type, List );
  for (i = 0; i < List->n_values; i++) {
      line = List->value[i];
      if ( line == exclude ) continue;

      /* No more needed */
      /*
      Line = Plus->Line[line]; 	
      if ( Line == NULL ) continue;
      if ( !(type & Line->type) ) continue; 
      */

      Vect_read_line (map, Points, NULL, line);
      
      Vect_line_distance ( Points, ux, uy, uz, with_z, NULL, NULL, NULL, &new_dist, NULL, NULL);
      G_debug( 3, " line = %d distance = %f", line,  new_dist);
      if ((++gotone == 1) || (new_dist <= cur_dist)) {
	  if (new_dist == cur_dist)
	    {
	      /* TODO */  
	      /* choice = dig_center_check (map->Line, choice, a, ux, uy); */
	      continue;
	    }
	  
	  choice = line;
	  cur_dist = new_dist;
       }
  }
  
  G_debug( 3, "min distance found = %f", cur_dist);
  if (cur_dist > maxdist)
      choice = 0;

  Vect_destroy_list ( List );
  return (choice);
}

/*!
 \fn int Vect_find_area (
		    struct Map_info *map,
		    double x, double y)
 \brief find nearest area
 \return area number, 0 if not found
 \param Map_info structure, ux, uy
*/

/* original dig_point_to_area() in grass50 */
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

