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
#include "gis.h"
#include "Vect.h"
#include <stdlib.h>

/* INTERFACE LEVEL II  */

int 
Vect_get_num_nodes (struct Map_info *map)
{
  return (map->plus.n_nodes);
}


int 
Vect_get_num_lines (struct Map_info *map)
{
  return (map->plus.n_lines);
}

int 
Vect_get_num_areas (struct Map_info *map)
{
  return (map->plus.n_areas);
}

int 
Vect_get_num_islands (struct Map_info *map)
{
  return (map->plus.n_isles);
}

int 
Vect_get_node_coor (struct Map_info *map, int num, double *x, double *y, double *z)
{
    P_NODE *Node;

    Node = map->plus.Node[num];
    *x = Node->x;
    *y = Node->y;

    if ( z != NULL )
        *z = Node->z;
  
    return (0);
}

/* get Line starting and ending node */
int 
Vect_get_line_nodes ( struct Map_info *Map, int line, int *n1, int *n2)
{

    if ( Map->level < 2 )
	G_fatal_error ("Map %s@%s is not open on level >= 2\n", Map->name, Map->mapset);
    
    if ( n1 != NULL ) 
	*n1 = Map->plus.Line[line]->N1;

    if ( n2 != NULL ) 
	*n2 = Map->plus.Line[line]->N2;

    return 1;
}

/* get areas/isles on the left and right */
int 
Vect_get_line_areas ( struct Map_info *Map, int line, int *left, int *right)
{

    if ( Map->level < 2 )
	G_fatal_error ("Map %s@%s is not open on level >= 2\n", Map->name, Map->mapset);
    
    if ( left != NULL ) 
	*left = Map->plus.Line[line]->left;

    if ( right != NULL ) 
	*right = Map->plus.Line[line]->right;

    return 1;
}

/* returns number of lines for node */
int 
Vect_get_node_n_lines ( struct Map_info *Map, int node )
{

    if ( Map->level < 2 )
	G_fatal_error ("Map %s@%s is not open on level >= 2\n", Map->name, Map->mapset);
    
    return ( Map->plus.Node[node]->n_lines );

}

/* TODO: fix explanation - difference to Vect_get_node_n_lines? */
/* returns number of lines for node */
int 
Vect_get_node_line ( struct Map_info *Map, int node, int line )
{

    if ( Map->level < 2 )
	G_fatal_error ("Map %s@%s is not open on level >= 2\n", Map->name, Map->mapset);
    
    return ( Map->plus.Node[node]->lines[line] );

}


