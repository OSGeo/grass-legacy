/*
* $Id$
*
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

/* 
*  Vect_select_lines_by_box ()
*  
*  Select lines by box. Select lines whose boxes overlap specified box!!!
*  It means that selected line may or may not overlap the box.
*  
*  argument 'list' must be initialized
*  
*  returns: number of areas
*           
*/
int 
Vect_select_lines_by_box (struct Map_info *Map, BOUND_BOX *Box, 
	                            int type, struct ilist *list)
{
    int       i, j, line, node, ret;
    struct    Plus_head *plus ;
    BOUND_BOX lbox;
    P_LINE    *Line;
    P_NODE    *Node;
    struct ilist *LocList;
    
    G_debug ( 3, "Vect_select_lines_by_box()" );
    G_debug ( 3, "Box(N,S,E,W,T,B): %e, %e, %e, %e, %e, %e", Box->N, Box->S,
                           Box->E, Box->W, Box->T, Box->B);
    plus = &(Map->plus);
    list->n_values = 0; 
    
    if ( ! (type & GV_LINES) ) { /* points and/or centroids only */
	LocList = Vect_new_list ();
	dig_select_nodes ( plus, Box, LocList );
        G_debug ( 5, "%d nodes selected", LocList->n_values );
	for ( i = 0; i < LocList->n_values; i++) {
            node = LocList->value[i];
	    Node = plus->Node[node];
	    for (j = 0; j < Node->n_lines; j++) {
		line = abs ( Node->lines[j] );
	        Line = plus->Line[line];
		if ( Line->type & type ) {
		     dig_list_add ( list, line );
		}
	    }
	}
    } else {
	for (line = 1; line <= plus->n_lines; line++) {
	    Line = plus->Line[line];
	    if ( !(Line->type & type) ) continue;
	    ret = Vect_get_line_box (Map, line, &lbox );
	    if ( ret == 1 ) { /* alive */
		if ( Vect_box_overlap( &lbox, Box ) ) {
		     dig_list_add ( list, line );
		}
	    }
	}
    }
    
    return list->n_values;
}
/* 
*  Vect_select_areas_by_box ()
*  
*  Select areas by box. Select areas whose boxes overlap specified box!!!
*  It means that selected area may or may not overlap the box.
*  
*  argument 'list' must be initialized
*  
*  returns: number of areas
*           
*/
int 
Vect_select_areas_by_box (struct Map_info *Map, BOUND_BOX *Box, struct ilist *list)
{
    int       area, ret;
    struct    Plus_head *plus ;
    BOUND_BOX abox;
    
    G_debug ( 3, "Vect_select_areas_by_box()" );
    G_debug ( 3, "Box(N,S,E,W,T,B): %e, %e, %e, %e, %e, %e", Box->N, Box->S,
                           Box->E, Box->W, Box->T, Box->B);
    plus = &(Map->plus);
    list->n_values = 0; 
    
    for (area = 1; area <= plus->n_areas; area++) {
	ret = Vect_get_area_box (Map, area, &abox );
	if ( ret == 1 ) { /* alive */
	    if ( Vect_box_overlap( &abox, Box ) ) {
		 dig_list_add ( list, area );
            }
	}
    }
    
    return list->n_values;
}

/* 
*  Vect_select_isles_by_box ()
*  
*  Select isles by box. Select isles whose boxes overlap specified box!!!
*  It means that selected isle may or may not overlap the box.
*  
*  argument 'list' must be initialized
*  
*  returns: number of areas
*           
*/
int 
Vect_select_isles_by_box (struct Map_info *Map, BOUND_BOX *Box, struct ilist *list)
{
    int       isle, ret;
    struct    Plus_head *plus ;
    BOUND_BOX ibox;
    
    G_debug ( 3, "Vect_select_isles_by_box()" );
    G_debug ( 3, "Box(N,S,E,W,T,B): %e, %e, %e, %e, %e, %e", Box->N, Box->S,
                           Box->E, Box->W, Box->T, Box->B);
    plus = &(Map->plus);
    list->n_values = 0; 
    
    for (isle = 1; isle <= plus->n_isles; isle++) {
	ret = Vect_get_isle_box (Map, isle, &ibox );
	if ( ret == 1 ) { /* alive */
	    if ( Vect_box_overlap( &ibox, Box ) ) {
		 dig_list_add ( list, isle );
            }
	}
    }
    
    return list->n_values;
}

/* 
*  Vect_select_nodes_by_box ()
*  
*  Select nodes by box. 
*  
*  argument 'list' must be initialized
*  
*  returns: number of nodes
*           
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
    
    return ( dig_select_nodes ( plus, Box, list ) ) ;
}
