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
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"

/* 
*  dig_spindex_init ()
*  initit spatial index (nodes, lines, areas, isles)
*
*  returns 1 OK
*          0 on error      
*/
int 
dig_spidx_init ( struct Plus_head *Plus) 
{
    
    G_debug(1, "dig_spidx_init()");
    
    Plus->Node_spidx = RTreeNewIndex();
    Plus->Line_spidx = RTreeNewIndex();
    Plus->Area_spidx = RTreeNewIndex();
    Plus->Isle_spidx = RTreeNewIndex();

    return 1;
}

/************************* ADD NEW *********************************/
/* 
*  dig_spindex_add_node ()
*  add new node to spatial index 
*
*  returns 1 OK
*          0 on error      
*/
int 
dig_spidx_add_node ( struct Plus_head *Plus, int node, 
	               double x, double y, double z) 
{
    struct Rect rect;
    
    G_debug(3, "dig_spidx_add_node(): node = %d, x,y,z = %f, %f, %f", node, x, y, z );

    rect.boundary[0] = x; rect.boundary[1] = y; rect.boundary[2] = z;
    rect.boundary[3] = x; rect.boundary[4] = y; rect.boundary[5] = z;
    RTreeInsertRect( &rect, node, &(Plus->Node_spidx), 0);

    return 1;
}

/* 
*  dig_spindex_add_line ()
*  add new line to spatial index 
*
*  returns 1 OK
*          0 on error      
*/
int 
dig_spidx_add_line ( struct Plus_head *Plus, int line, BOUND_BOX *box ) 
{
    struct Rect rect;
    
    G_debug(3, "dig_spidx_add_line(): line = %d", line );

    rect.boundary[0] = box->W; rect.boundary[1] = box->S; rect.boundary[2] = box->B;
    rect.boundary[3] = box->E; rect.boundary[4] = box->N; rect.boundary[5] = box->T;
    RTreeInsertRect( &rect, line, &(Plus->Line_spidx), 0);

    return 0;
}

/* 
*  dig_spindex_add_area ()
*  add new area to spatial index 
*
*  returns 1 OK
*          0 on error      
*/
int 
dig_spidx_add_area ( struct Plus_head *Plus, int area, BOUND_BOX *box ) 
{
    struct Rect rect;
    
    G_debug(3, "dig_spidx_add_area(): area = %d", area );

    rect.boundary[0] = box->W; rect.boundary[1] = box->S; rect.boundary[2] = box->B;
    rect.boundary[3] = box->E; rect.boundary[4] = box->N; rect.boundary[5] = box->T;
    RTreeInsertRect( &rect, area, &(Plus->Area_spidx), 0);

    return 0;
}

/* 
*  dig_spindex_add_isle ()
*  add new island to spatial index 
*
*  returns 1 OK
*          0 on error      
*/

int 
dig_spidx_add_isle ( struct Plus_head *Plus, int isle, BOUND_BOX *box ) 
{
    struct Rect rect;
    
    G_debug(3, "dig_spidx_add_isle(): isle = %d", isle );

    rect.boundary[0] = box->W; rect.boundary[1] = box->S; rect.boundary[2] = box->B;
    rect.boundary[3] = box->E; rect.boundary[4] = box->N; rect.boundary[5] = box->T;
    RTreeInsertRect( &rect, isle, &(Plus->Isle_spidx), 0);

    return 0;
}

/* 
*  dig_spindex_del_node ()
*  delete node from spatial index 
*
*  returns 1 OK
*          0 on error      
*/
int 
dig_spidx_del_node ( struct Plus_head *Plus, int node ) 
{
    int    ret;
    P_NODE *Node; 
    struct Rect rect;
    
    G_debug(3, "dig_spidx_del_node(): node = %d", node );

    Node = Plus->Node[node];

    rect.boundary[0] = Node->x; rect.boundary[1] = Node->y; rect.boundary[2] = Node->z;
    rect.boundary[3] = Node->x; rect.boundary[4] = Node->y; rect.boundary[5] = Node->z;
    
    ret = RTreeDeleteRect( &rect, node, &(Plus->Node_spidx) ); 

    if ( ret ) G_fatal_error ( "Cannot delete node %d from spatial index", node );
    
    return 0;
}

/* 
*  dig_spindex_del_line ()
*  delete line from spatial index 
*
*  returns 1 OK
*          0 on error      
*/
int 
dig_spidx_del_line ( struct Plus_head *Plus, int line ) 
{
    P_LINE *Line; 
    struct Rect rect;
    int ret;
    
    G_debug(3, "dig_spidx_del_line(): line = %d", line );

    Line = Plus->Line[line];

    G_debug(3, "  box(x1,y1,z1,x2,y2,z2): %f %f %f %f %f %f",Line->W,Line->S,Line->B,Line->E,Line->N, Line->T );

    rect.boundary[0] = Line->W; rect.boundary[1] = Line->S; rect.boundary[2] = Line->B;
    rect.boundary[3] = Line->E; rect.boundary[4] = Line->N; rect.boundary[5] = Line->T;
    
    ret = RTreeDeleteRect( &rect, line, &(Plus->Line_spidx) ); 
    
    G_debug(3, "  ret = %d", ret );
    
    if ( ret ) G_fatal_error ( "Cannot delete line %d from spatial index", line );

    return 0;
}

/* 
*  dig_spindex_del_area ()
*  delete area from spatial index 
*
*  returns 1 OK
*          0 on error      
*/
int 
dig_spidx_del_area ( struct Plus_head *Plus, int area ) 
{
    int ret;
    P_AREA *Area; 
    struct Rect rect;
    
    G_debug(3, "dig_spidx_del_area(): area = %d", area );

    Area = Plus->Area[area];

    if (Area == NULL) {
	G_fatal_error ("Attempt to delete sidx for dead area");
    }

    rect.boundary[0] = Area->W; rect.boundary[1] = Area->S; rect.boundary[2] = Area->B;
    rect.boundary[3] = Area->E; rect.boundary[4] = Area->N; rect.boundary[5] = Area->T;
    
    ret = RTreeDeleteRect( &rect, area, &(Plus->Area_spidx) ); 

    if ( ret ) G_fatal_error ( "Cannot delete area %d from spatial index", area );
    
    return 0;
}

/* 
*  dig_spindex_del_isle ()
*  delete isle from spatial index 
*
*  returns 1 OK
*          0 on error      
*/
int 
dig_spidx_del_isle ( struct Plus_head *Plus, int isle ) 
{
    int ret;
    P_ISLE *Isle; 
    struct Rect rect;
    
    G_debug(3, "dig_spidx_del_isle(): isle = %d", isle );

    Isle = Plus->Isle[isle];

    rect.boundary[0] = Isle->W; rect.boundary[1] = Isle->S; rect.boundary[2] = Isle->B;
    rect.boundary[3] = Isle->E; rect.boundary[4] = Isle->N; rect.boundary[5] = Isle->T;

    ret = RTreeDeleteRect( &rect, isle, &(Plus->Isle_spidx) ); 

    if ( ret ) G_fatal_error ( "Cannot delete isle %d from spatial index", isle );
    
    return 0;
}
/************************* SELECT BY BOX *********************************/
/* This function is called by  RTreeSearch() to add selected node/line/area/isle to thelist */
int _add_item(int id, struct ilist *list)
{
    dig_list_add ( list, id );
    return 1;
}
/* 
*  dig_select_nodes ()
*  select nodes by bbox 
*
*  returns: number of selected nodes
*           -1 error
*/
int 
dig_select_nodes ( struct Plus_head *Plus, BOUND_BOX *box, struct ilist *list ) 
{
    struct Rect rect;
    
    G_debug(3, "dig_select_nodes()" );
    
    list->n_values = 0;

    rect.boundary[0] = box->W; rect.boundary[1] = box->S; rect.boundary[2] = box->B;
    rect.boundary[3] = box->E; rect.boundary[4] = box->N; rect.boundary[5] = box->T;
    RTreeSearch( Plus->Node_spidx, &rect, (void *) _add_item, list);
    
    return ( list->n_values );
}

/* This function is called by  RTreeSearch() for nodes to add selected node to list */
int _add_node(int id, int *node)
{
    *node = id;
    return 0;
}

/* 
*  dig_find_node ()
*  find one node by coordinates 
*
*  returns: number of node
*           0 not found
*/
int 
dig_find_node ( struct Plus_head *Plus, double x, double y, double z ) 
{
    struct Rect rect;
    struct ilist list;
    int    node;
    
    G_debug(3, "dig_find_node()" );
    
    dig_init_list ( &list );
    
    rect.boundary[0] = x; rect.boundary[1] = y; rect.boundary[2] = z;
    rect.boundary[3] = x; rect.boundary[4] = y; rect.boundary[5] = z;

    node = 0; 
    RTreeSearch( Plus->Node_spidx, &rect, (void *) _add_node, &node);

    return node;
}

/* 
*  dig_select_lines ()
*  select lines by box 
*
*  returns: number of selected lines
*/
int 
dig_select_lines ( struct Plus_head *Plus, BOUND_BOX *box, struct ilist *list ) 
{
    struct Rect rect;
    
    G_debug(3, "dig_select_lines()" );
    
    list->n_values = 0;

    rect.boundary[0] = box->W; rect.boundary[1] = box->S; rect.boundary[2] = box->B;
    rect.boundary[3] = box->E; rect.boundary[4] = box->N; rect.boundary[5] = box->T;
    RTreeSearch( Plus->Line_spidx, &rect, (void *) _add_item, list);
    
    return ( list->n_values );
}

/* 
*  dig_select_areas ()
*  select areas by box 
*
*  returns: number of selected areas
*/
int 
dig_select_areas ( struct Plus_head *Plus, BOUND_BOX *box, struct ilist *list ) 
{
    struct Rect rect;
    
    G_debug(3, "dig_select_areas()" );
    
    list->n_values = 0;

    rect.boundary[0] = box->W; rect.boundary[1] = box->S; rect.boundary[2] = box->B;
    rect.boundary[3] = box->E; rect.boundary[4] = box->N; rect.boundary[5] = box->T;
    RTreeSearch( Plus->Area_spidx, &rect, (void *) _add_item, list);
    
    return ( list->n_values );
}

/* 
*  dig_select_isles ()
*  select isles by box 
*
*  returns: number of selected isles
*/
int 
dig_select_isles ( struct Plus_head *Plus, BOUND_BOX *box, struct ilist *list ) 
{
    struct Rect rect;
    
    G_debug(3, "dig_select_isles()" );
    
    list->n_values = 0;

    rect.boundary[0] = box->W; rect.boundary[1] = box->S; rect.boundary[2] = box->B;
    rect.boundary[3] = box->E; rect.boundary[4] = box->N; rect.boundary[5] = box->T;
    RTreeSearch( Plus->Isle_spidx, &rect, (void *) _add_item, list);
    
    return ( list->n_values );
}


