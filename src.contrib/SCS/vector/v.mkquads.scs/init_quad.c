
/*
*  Deduces what lat, long quad we are in.
*  Written by GRASS, Fall of 88,  Michael H.
*/

#include	<stdio.h>
#include	"gis.h"
#include	"quad_structs.h"


init_quad_struct (quads_info, W_ll)
	struct  quads_description  *quads_info ;
	struct  Cell_head  *W_ll ;
{

		quads_info->lat_shift =  QUAD_SIZE ;
		quads_info->lon_shift =  QUAD_SIZE ;


	return(0) ;

}

