/****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Radim Blazek
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
#include "glocale.h"
#include "gis.h"
#include "Vect.h"

/*!
  \fn void Vect_spatial_index_init ( SPATIAL_INDEX *si )
  \brief init spatial index
  \param  si pointer to spatial index structure
*/
void 
Vect_spatial_index_init ( SPATIAL_INDEX *si ) 
{
    G_debug(1, "Vect_spatial_index_init()");
    
    si->root = RTreeNewIndex();
}

/*!
  \fn void Vect_spatial_index_destroy ( SPATIAL_INDEX *si )
  \brief destroy existing spatial index, Vect_spatial_index_init() must be call before new use
  \param  si pointer to spatial index structure
*/
void 
Vect_spatial_index_destroy ( SPATIAL_INDEX *si ) 
{
    G_debug(1, "Vect_spatial_index_destroy()");
    
    RTreeDestroyNode ( si->root );
}

/*!
  \fn void Vect_spatial_index_add_item ( SPATIAL_INDEX *si, int id, BOUND_BOX *box )
  \brief Add a new item to spatial index
  \param  si pointer to spatial index structure
  \param id item identifier
  \param box pointer to item bounding box
*/
void 
Vect_spatial_index_add_item ( SPATIAL_INDEX *si, int id, BOUND_BOX *box ) 
{
    struct Rect rect;
    
    G_debug(3, "Vect_spatial_index_add_item(): id = %d", id );

    rect.boundary[0] = box->W; rect.boundary[1] = box->S; rect.boundary[2] = box->B;
    rect.boundary[3] = box->E; rect.boundary[4] = box->N; rect.boundary[5] = box->T;
    RTreeInsertRect( &rect, id, &(si->root), 0);
}

/*!
  \fn void Vect_spatial_index_del_item ( SPATIAL_INDEX *si, int id )
  \brief Delete item from spatial index
  \param  si pointer to spatial index structure
  \param id item identifier
*/
void 
Vect_spatial_index_del_item ( SPATIAL_INDEX *si, int id ) 
{
    int    ret;
    struct Rect rect;
    
    G_debug(3, "Vect_spatial_index_del_item(): id = %d", id );

    /* TODO */
    G_fatal_error ( "Vect_spatial_index_del_item() not implemented" );

    /* Bounding box of item would be needed, which is not stored in si. */
    
    /* 
    rect.boundary[0] = ; rect.boundary[1] = ; rect.boundary[2] = ;
    rect.boundary[3] = ; rect.boundary[4] = ; rect.boundary[5] = ;
    */
    
    ret = RTreeDeleteRect( &rect, id, &(si->root) ); 

    if ( ret ) G_fatal_error ( "Cannot delete item %d from spatial index", id );
}

/*!
  \brief Create spatial index if necessary. To be used in modules.
         Map must be opened on level 2.
  \param Map pointer to map
  \param out print progress here
  \return 0 OK
  \return 1 error
*/
int 
Vect_build_spatial_index ( struct Map_info *Map, FILE *msgout ) 
{
    if ( Map->level < 2 ) {
	G_fatal_error ( _( "Cannot build spatial index from topology, the map is not opened on level2"));
    }
    if ( !(Map->plus.Spidx_built) ) {
	return ( Vect_build_sidx_from_topo ( Map, msgout ) );
    }
    return 0;
}

/*!
  \brief Create spatial index from topo if necessary
  \param Map pointer to map
  \param out print progress here
  \return 0 OK
  \return 1 error
*/
int 
Vect_build_sidx_from_topo ( struct Map_info *Map, FILE *msgout ) 
{
    int  i, total, done;
    struct Plus_head *plus;
    BOUND_BOX box;
    P_LINE *Line;
    P_NODE *Node;
    P_AREA *Area;
    P_ISLE *Isle;

    G_debug ( 3, "Vect_build_sidx_from_topo()" );

    plus = &(Map->plus);

    dig_spidx_init ( plus );

    total = plus->n_nodes + plus->n_lines + plus->n_areas + plus->n_isles;

    /* Nodes */
    for (i = 1; i <= plus->n_nodes; i++) {
	G_percent2 ( i, total, 1, msgout );
	
	Node = plus->Node[i];
	if ( !Node ) G_fatal_error ("BUG (Vect_build_sidx_from_topo): node does not exist");

	dig_spidx_add_node ( plus, i, Node->x, Node->y, Node->z );
    }

    /* Lines */
    done = plus->n_nodes;
    for (i = 1; i <= plus->n_lines; i++) {
	G_percent2 ( done+i, total, 1, msgout );

	Line = plus->Line[i];
	if ( !Line ) G_fatal_error ("BUG (Vect_build_sidx_from_topo): line does not exist");

	box.N = Line->N;
	box.S = Line->S;
	box.E = Line->E;
	box.W = Line->W;
	box.T = Line->T;
	box.B = Line->B;

	dig_spidx_add_line ( plus, i, &box );
    }
    
    /* Areas */
    done += plus->n_lines;
    for (i = 1; i <= plus->n_areas; i++) {
	G_percent2 ( done+i, total, 1, msgout );
	
	Area = plus->Area[i];
	if ( !Area ) G_fatal_error ("BUG (Vect_build_sidx_from_topo): area does not exist");

	box.N = Area->N;
	box.S = Area->S;
	box.E = Area->E;
	box.W = Area->W;
	box.T = Area->T;
	box.B = Area->B;

	dig_spidx_add_area ( plus, i, &box );
    }

    /* Isles */
    done += plus->n_areas;
    for (i = 1; i <= plus->n_isles; i++) {
	G_percent2 ( done+i, total, 1, msgout );

	Isle = plus->Isle[i];
	if ( !Isle ) G_fatal_error ("BUG (Vect_build_sidx_from_topo): isle does not exist");

	box.N = Isle->N;
	box.S = Isle->S;
	box.E = Isle->E;
	box.W = Isle->W;
	box.T = Isle->T;
	box.B = Isle->B;

	dig_spidx_add_isle ( plus, i, &box );
    }

    Map->plus.Spidx_built = 1;
    
    G_debug ( 3, "Spatial index was built" );

    return 0;
}

/************************* SELECT BY BOX *********************************/
/* This function is called by  RTreeSearch() to add selected item to the list */
static int _add_item(int id, struct ilist *list)
{
    dig_list_add ( list, id );
    return 1;
}

/*!
  \fn int Vect_spatial_index_select ( SPATIAL_INDEX *si, BOUND_BOX *box, struct ilist *list )
  \brief Select items by bounding box to list
  \return number of selected items
  \param  si pointer to spatial index structure
  \param box bounding box
  \param list pointer to list where selected items are stored
*/
int
Vect_spatial_index_select ( SPATIAL_INDEX *si, BOUND_BOX *box, struct ilist *list ) 
{
    struct Rect rect;
    
    G_debug(3, "Vect_spatial_index_select()" );
    
    list->n_values = 0;

    rect.boundary[0] = box->W; rect.boundary[1] = box->S; rect.boundary[2] = box->B;
    rect.boundary[3] = box->E; rect.boundary[4] = box->N; rect.boundary[5] = box->T;
    RTreeSearch( si->root, &rect, (void *) _add_item, list);
    
    G_debug(3, "  %d items selected", list->n_values );
    return ( list->n_values );
}

