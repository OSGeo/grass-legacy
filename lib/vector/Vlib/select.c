/*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Radim Blazek
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

/*!
 \fn int Vect_select_lines_by_box (struct Map_info *Map, BOUND_BOX *Box, 
	                            int type, struct ilist *list)
 \brief Select lines by box. Select lines whose boxes overlap specified box!!!
  It means that selected line may or may not overlap the box.
  Argument 'list' must be initialized.
 \return number of areas
 \param Map_info structure, BOUND_BOX , vector type?, ilist
*/
int 
Vect_select_lines_by_box (struct Map_info *Map, BOUND_BOX *Box, 
	                            int type, struct ilist *list)
{
    int       i, line, nlines;
    struct    Plus_head *plus ;
    P_LINE    *Line;
    struct     ilist *LocList;
    
    G_debug ( 3, "Vect_select_lines_by_box()" );
    G_debug ( 3, "  Box(N,S,E,W,T,B): %e, %e, %e, %e, %e, %e", Box->N, Box->S,
                           Box->E, Box->W, Box->T, Box->B);
    plus = &(Map->plus);
    list->n_values = 0; 
    LocList = Vect_new_list ();
    
    nlines = dig_select_lines ( &(Map->plus), Box, LocList );
    G_debug ( 3, "  %d lines selected (all types)", nlines );

    /* Remove lines of not requested types */
    for ( i = 0; i < nlines; i++ ) { 
	line = LocList->value[i];
	if (  plus->Line[line] == NULL ) continue; /* Should not happen */
        Line = plus->Line[line];
	if ( !(Line->type & type) ) continue; 
	dig_list_add ( list, line ); 
    }

    Vect_destroy_list ( LocList );    
        
    G_debug ( 3, "  %d lines of requested type", list->n_values );
    
    return list->n_values;
}

/*!
 \fn int Vect_select_areas_by_box (struct Map_info *Map, BOUND_BOX *Box, struct ilist *list)
 \brief Select areas by box. Select areas whose boxes overlap specified box!!!
  It means that selected area may or may not overlap the box.
  Argument 'list' must be initialized
 \return number of areas
 \param Map_info structure, BOUND_BOX , vector type?, ilist
*/
int 
Vect_select_areas_by_box (struct Map_info *Map, BOUND_BOX *Box, struct ilist *list)
{
    int i;
    
    G_debug ( 3, "Vect_select_areas_by_box()" );
    G_debug ( 3, "Box(N,S,E,W,T,B): %e, %e, %e, %e, %e, %e", Box->N, Box->S,
                           Box->E, Box->W, Box->T, Box->B);

    dig_select_areas ( &(Map->plus), Box, list );
    G_debug ( 3, "  %d areas selected", list->n_values );
    for ( i = 0; i < list->n_values; i++ ) {
        G_debug ( 3, "  %d : %d", list->value[i], Map->plus.Area[list->value[i]] );
            
    }
    return list->n_values;
}


/*!
 \fn  int Vect_select_isles_by_box (struct Map_info *Map, BOUND_BOX *Box, struct ilist *list)
 \brief   Select isles by box. Select isles whose boxes overlap specified box!!!
    It means that selected isle may or may not overlap the box.
    Argument 'list' must be initialized
 \return number of isles
 \param Map_info structure
*/
int 
Vect_select_isles_by_box (struct Map_info *Map, BOUND_BOX *Box, struct ilist *list)
{
    G_debug ( 3, "Vect_select_isles_by_box()" );
    G_debug ( 3, "Box(N,S,E,W,T,B): %e, %e, %e, %e, %e, %e", Box->N, Box->S,
                           Box->E, Box->W, Box->T, Box->B);
    
    dig_select_isles ( &(Map->plus), Box, list );
    G_debug ( 3, "  %d isles selected", list->n_values );
    
    return list->n_values;
}

/*!
 \fn int Vect_select_nodes_by_box (struct Map_info *Map, BOUND_BOX *Box, struct ilist *list)
 \brief Select nodes by box.
   Argument 'list' must be initialized
 \return number of nodes
 \param Map_info structure, BOUND_BOX, ilist
*/
int 
Vect_select_nodes_by_box (struct Map_info *Map, BOUND_BOX *Box, struct ilist *list)
{
    struct    Plus_head *plus ;
    
    G_debug ( 3, "Vect_select_nodes_by_box()" );
    G_debug ( 3, "Box(N,S,E,W,T,B): %e, %e, %e, %e, %e, %e", Box->N, Box->S,
                           Box->E, Box->W, Box->T, Box->B);
    plus = &(Map->plus);
    list->n_values = 0; 
    
    dig_select_nodes ( plus, Box, list );
    G_debug ( 3, "  %d nodes selected", list->n_values );
    
    return list->n_values;
}

