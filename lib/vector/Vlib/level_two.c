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
/*
   **
   **INTERFACE LEVEL II
   **==================
   **
 */

#include "gis.h"
#include "Vect.h"
#include <stdlib.h>


static int Writable = 0;	/* Open Plus file for WRITE/READONLY */
static char *RW_str = "r";


int 
Vect_P_init (
	      char *name,
	      char *mapset,
	      struct Map_info *map)
{
  char *error;
/*
  if (NULL != (error = Vect__P_init (map, name, mapset)))
    {
      fprintf (stderr, "%s\n", error);
      exit (-1);
    }
*/
  return (0);
}

int 
V2_num_nodes (struct Map_info *map)
{
  return (map->plus.n_nodes);
}


int 
V2_num_lines (struct Map_info *map)
{
  return (map->plus.n_lines);
}

int 
V2_num_areas (struct Map_info *map)
{
  return (map->plus.n_areas);
}

int 
Vect_get_node_point (struct Map_info *map, int num, double *x, double *y)
{
    P_NODE *Node;

    Node = map->plus.Node[num];
    *x = Node->x;
    *y = Node->y;
  
    return (0);
}



/* get Area bounding box info in NSEW */
int 
V2_get_area_bbox (struct Map_info *map, int area,
		  double *N, double *S, double *E, double *W)
{
  P_AREA *Area;
/*
  if (area <= 0 || area > map->plus.n_areas)
    return (-1);
  if (!AREA_ALIVE (&(map->plus.Area[area])))
    return (-1);
  Area = &(map->plus.Area[area]);
  *N = Area->N;
  *E = Area->E;
  *W = Area->W;
  *S = Area->S;
  */
  return (0);
}

/* get Line bounding box info in NSEW */
int 
V2_get_line_bbox (
		   struct Map_info *map, int line,
		   double *N, double *S, double *E, double *W)
{
  P_LINE *Line;
/*
  if (line <= 0 || line > map->plus.n_lines)
    return (-1);
  if (!LINE_ALIVE (&(map->plus.Line[line])))
    return (-1);
  Line = &(map->plus.Line[line]);
  *N = Line->N;
  *E = Line->E;
  *W = Line->W;
  *S = Line->S;
*/
  return (0);
}
