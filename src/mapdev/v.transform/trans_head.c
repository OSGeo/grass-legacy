#include <stdio.h>
#include "Vect.h"
#include "libtrans.h"

static int _transform_head_info (struct dig_head *);

int 
transform_head_info (struct dig_head *dhead)
{
	return _transform_head_info( dhead ) ;
}

/*
*  Zero out any information that is no longer valid under the new
*  map coordinates.  Convert the window in the heading to the new
*  map coordinate system.  Since transform_a_into_b() works with pairs
*  we pair up the window coordinates. (west and north,  east and south).
*/

static int 
_transform_head_info (struct dig_head *header)
{
	double  east, north ;

	header->orig_scale  =  0.0 ;
	header->plani_zone  =  0 ;
	header->map_thresh  =  0.0 ;

	east  = header->W ;
	north = header->N ;
	transform_a_into_b( east, north, &header->W, &header->N ) ;

	east  = header->E ;
	north = header->S ;
	transform_a_into_b( east, north, &header->E, &header->S ) ;

	return 0;
}
