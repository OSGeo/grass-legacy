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
#include "gis.h"
#include "Vect.h"
#include <stdlib.h>

/* INTERFACE LEVEL II  */

/*!
 \fn int Vect_get_num_nodes (struct Map_info *map)
 \brief get number of nodes
 \return number of nodes
 \param Map_info structure
 */
int 
Vect_get_num_nodes (struct Map_info *map)
{
  return (map->plus.n_nodes);
}

/*!
 \fn int Vect_get_num_centroids (struct Map_info *map)
 \brief get number of centroids
 \return number of centroids
 \param Map_info structure
 */
int 
Vect_get_num_centroids (struct Map_info *map)
{
  return (map->plus.n_clines);
}


/*!
 \fn int Vect_get_num_lines (struct Map_info *map)
 \brief get number of lines
 \return number of lines
 \param Map_info structure
 */
int 
Vect_get_num_lines (struct Map_info *map)
{
  return (map->plus.n_lines);
}

/*!
 \fn int Vect_get_num_areas (struct Map_info *map)
 \brief get number of areas
 \return number of areas
 \param Map_info structure
 */
int 
Vect_get_num_areas (struct Map_info *map)
{
  return (map->plus.n_areas);
}

/*!
 \fn int Vect_get_num_islands (struct Map_info *map)
 \brief get number of islands
 \return number of islands
 \param Map_info structure
 */
int 
Vect_get_num_islands (struct Map_info *map)
{
  return (map->plus.n_isles);
}

/*!
 \fn int Vect_get_num_updated_lines (struct Map_info *map)
 \brief get number of updated lines
 \return number of updated lines
 \param Map_info structure
 */
int 
Vect_get_num_updated_lines (struct Map_info *map)
{
  return (map->plus.n_uplines);
}

/*!
 \fn int Vect_get_updated_line (struct Map_info *map, int idx)
 \brief get updated line by index
 \return updated line
 \param Map_info structure
 */
int 
Vect_get_updated_line (struct Map_info *map, int idx)
{
  return (map->plus.uplines[idx]);
}

/*!
 \fn int Vect_get_num_updated_nodes (struct Map_info *map)
 \brief get number of updated nodes
 \return number of updated nodes
 \param Map_info structure
 */
int 
Vect_get_num_updated_nodes (struct Map_info *map)
{
  return (map->plus.n_upnodes);
}

/*!
 \fn int Vect_get_updated_node (struct Map_info *map, int idx)
 \brief get updated node by index
 \return updated node
 \param Map_info structure
 */
int 
Vect_get_updated_node (struct Map_info *map, int idx)
{
  return (map->plus.upnodes[idx]);
}

/*!
 \fn int Vect_get_node_coor (struct Map_info *map, int num, double *x, double *y, double *z)
 \brief get 2D/3D coordinates of node
 \return 2D/3D coordinates of node
 \param Map_info structure, node number, xyz coordinates values
 */
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

/*!
 \fn int Vect_get_line_nodes ( struct Map_info *Map, int line, int *n1, int *n2)
 \brief get starting and ending node of line
 \return numbers of line nodes
 \param Map_info structure, line number, numbers of line nodes
*/
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

/*!
 \fn int Vect_get_line_areas ( struct Map_info *Map, int line, int *left, int *right)
 \brief get areas/isles on the left and right
 \return numbers of areas/isles on the left and right
 \param Map_info structure, line number, numbers of areas/isles on the left and right
*/
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

/*!
 \fn int Vect_get_node_n_lines ( struct Map_info *Map, int node )
 \brief returns number of lines for node
 \return numbers of line for a node ??
 \param Map_info structure, node number
*/
int 
Vect_get_node_n_lines ( struct Map_info *Map, int node )
{

    if ( Map->level < 2 )
	G_fatal_error ("Map %s@%s is not open on level >= 2\n", Map->name, Map->mapset);
    
    return ( Map->plus.Node[node]->n_lines );

}

/* TODO: fix explanation - difference to Vect_get_node_n_lines? */
/*!
 \fn int Vect_get_node_line ( struct Map_info *Map, int node, int line )
 \brief returns number of lines for node
 \return numbers of line for a node ??
 \param Map_info structure, node number, line number
*/
int 
Vect_get_node_line ( struct Map_info *Map, int node, int line )
{
    if ( Map->level < 2 )
	G_fatal_error ("Map %s@%s is not open on level >= 2\n", Map->name, Map->mapset);
    
    return ( Map->plus.Node[node]->lines[line] );
}

/*!
 \fn int Vect_get_centroid_area ( struct Map_info *Map, int centroid )
 \brief returns number of area the centroid is within
 \return number of area the node is within, 0 for not in area, negative number of area if duplicate
 \param Map_info structure, centroid number
*/
int 
Vect_get_centroid_area ( struct Map_info *Map, int centroid )
{
    if ( Map->level < 2 )
	G_fatal_error ("Map %s@%s is not open on level >= 2\n", Map->name, Map->mapset);
    
    return ( Map->plus.Line[centroid]->left );
}
