/*
* $Id$
*
****************************************************************************
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
#include "Vect.h"

/* 
*  dig_line_box ()
*  set box to points extent
*/
int 
dig_line_box (struct line_pnts *Points, BOUND_BOX *Box){
    int  i;
  
    if ( Points->n_points <= 0 ) {
	Box->N = 0;
	Box->S = 0;
	Box->E = 0;
	Box->W = 0;
	Box->T = 0;
	Box->B = 0;
	return 0;
    }

    Box->E = Points->x[0];
    Box->W = Points->x[0];
    Box->N = Points->y[0];
    Box->S = Points->y[0];
    Box->T = Points->z[0];
    Box->B = Points->z[0];
    
    for ( i = 1; i < Points->n_points; i++ ) {
        if ( Points->x[i] > Box->E ) Box->E = Points->x[i];
	else if ( Points->x[i] < Box->W ) Box->W = Points->x[i];
	
        if ( Points->y[i] > Box->N ) Box->N = Points->y[i];
	else if ( Points->y[i] < Box->S ) Box->S = Points->y[i];
	
        if ( Points->z[i] > Box->T ) Box->T = Points->z[i];
	else if ( Points->z[i] < Box->B ) Box->B = Points->z[i];
    }

    return 1;
}

