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

