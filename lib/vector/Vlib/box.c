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
 \fn int Vect_point_in_box (double x, double y, double z, BOUND_BOX *Box)
 \brief tests for point in box
 \return 1 point is in box, 0 point is not in box
 \param xyz coordinates, boundary box
*/
int 
Vect_point_in_box (double x, double y, double z, BOUND_BOX *Box)
{
  
    if ( x >= Box->W && x <= Box->E &&
	 y >= Box->S && y <= Box->N &&
	 z >= Box->B && z <= Box->T )
    {
	return 1;
    }

    return 0;
}

/*!
 \fn int Vect_box_overlap (BOUND_BOX *A, BOUND_BOX *B)
 \brief tests for overlap of two boxes
 \return 1 boxes overlap, 0 boxes do not overlap
 \param boundary box A, boundary box B
*/
int 
Vect_box_overlap (BOUND_BOX *A, BOUND_BOX *B)
{

    if ( A->E < B->W || A->W > B->E ||
         A->N < B->S || A->S > B->N ||
         A->T < B->B || A->B > B->T )
    {
	return 0;
    }

    return 1;
}

/*!
 \fn int Vect_box_copy (BOUND_BOX *A, BOUND_BOX *B)
 \brief copy box A to box B
 \return 1
 \param boundary box1, boundary box2
*/
int 
Vect_box_copy (BOUND_BOX *A, BOUND_BOX *B)
{

    A->N = B->N;
    A->S = B->S;
    A->E = B->E;
    A->W = B->W;
    A->T = B->T;
    A->B = B->B;

    return 1;
}

/*!
 \fn int Vect_box_extend (BOUND_BOX *A, BOUND_BOX *B)
 \brief extend box A by box B
 \return 1
 \param boundary box1, boundary box2
*/
int 
Vect_box_extend (BOUND_BOX *A, BOUND_BOX *B)
{

    if ( B->N > A->N ) A->N = B->N;
    if ( B->S < A->S ) A->S = B->S;
    if ( B->E > A->E ) A->E = B->E;
    if ( B->W < A->W ) A->W = B->W;
    if ( B->T > A->T ) A->T = B->T;
    if ( B->B < A->B ) A->B = B->B;

    return 1;
}

/*!
 \fn int Vect_get_line_box (struct Map_info *Map, int line, BOUND_BOX *Box )
 \brief get line number in boundary box ??
 \return 1 on success, 0 line is dead
 \param Map_info structure, line number, boundary box
*/
int 
Vect_get_line_box (struct Map_info *Map, int line, BOUND_BOX *Box )
{
    struct    Plus_head *Plus;
    P_LINE    *Line;
    
    Plus = &(Map->plus);
    Line = Plus->Line[line];
    
    if ( Line == NULL ) {  /* dead */
        Box->N = 0;
        Box->S = 0;
        Box->E = 0;
        Box->W = 0;
        Box->T = 0;
        Box->B = 0;
	return 0;
    } else {
        Box->N = Line->N;
        Box->S = Line->S;
        Box->E = Line->E;
        Box->W = Line->W;
        Box->T = Line->T;
        Box->B = Line->B;
    }

    return 1;
}

/*!
 \fn int Vect_get_area_box (struct Map_info *Map, int area, BOUND_BOX *Box )
 \brief get area number in boundary box ?
 \return 1 on success, 0 area is dead
 \param Map_info structure, area number, boundary box
*/
int 
Vect_get_area_box (struct Map_info *Map, int area, BOUND_BOX *Box )
{
    struct    Plus_head *Plus;
    P_AREA    *Area;
    
    Plus = &(Map->plus);
    Area = Plus->Area[area];
    
    if ( Area == NULL ) {  /* dead */
        Box->N = 0;
        Box->S = 0;
        Box->E = 0;
        Box->W = 0;
        Box->T = 0;
        Box->B = 0;
	return 0;
    } else {
        Box->N = Area->N;
        Box->S = Area->S;
        Box->E = Area->E;
        Box->W = Area->W;
        Box->T = Area->T;
        Box->B = Area->B;
    }

    return 1;
}

/*!
 \fn int Vect_get_isle_box (struct Map_info *Map, int isle, BOUND_BOX *Box )
 \brief get isle number in boundary box ?
 \return 1 on success, 0 isle is dead
 \param Map_info structure, isle number, boundary box
*/
int 
Vect_get_isle_box (struct Map_info *Map, int isle, BOUND_BOX *Box )
{
    struct    Plus_head *Plus;
    P_ISLE    *Isle;
    
    Plus = &(Map->plus);
    Isle = Plus->Isle[isle];
    
    if ( Isle == NULL ) {  /* dead */
        Box->N = 0;
        Box->S = 0;
        Box->E = 0;
        Box->W = 0;
        Box->T = 0;
        Box->B = 0;
	return 0;
    } else {
        Box->N = Isle->N;
        Box->S = Isle->S;
        Box->E = Isle->E;
        Box->W = Isle->W;
        Box->T = Isle->T;
        Box->B = Isle->B;
    }

    return 1;
}

/*!
 \fn int Vect_get_map_box (struct Map_info *Map, BOUND_BOX *Box )
 \brief falls map into boundary box ???
 \return 1 on success, 0 on error
 \param Map_info structure, boundary box
*/
int 
Vect_get_map_box (struct Map_info *Map, BOUND_BOX *Box )
{
    struct    Plus_head *Plus;
    
    Plus = &(Map->plus);
    
    Box->N = Plus->box.N;
    Box->S = Plus->box.S;
    Box->E = Plus->box.E;
    Box->W = Plus->box.W;
    Box->T = Plus->box.T;
    Box->B = Plus->box.B;

    return 1;
}


/*!
 \fn int Vect_region_box (struct Cell_head *Window, BOUND_BOX *Box )
 \brief copy region Window to Box
 \return 1 on success, 0 on error
 \param region structure, boundary box
*/
int 
Vect_region_box ( struct Cell_head *Window, BOUND_BOX *Box )
{
    
    Box->N = Window->north;
    Box->S = Window->south;
    Box->E = Window->east;
    Box->W = Window->west;
    Box->T = PORT_DOUBLE_MAX;
    Box->B = -PORT_DOUBLE_MAX;

    return 1;
}

