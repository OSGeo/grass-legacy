
/*
*  Deduces what lat, long quad we are in.
*  Written by GRASS, Fall of 88,  Michael H.
*/

#include	<stdio.h>
#include	"gis.h"
#include	"quad_structs.h"


int init_quad_struct (struct quads_description *quads_info,
	struct Cell_head *W_ll, int hsize, int vsize)
{

	if (W_ll->north > W_ll->south) 
	{
		quads_info->north = 1 ;
		quads_info->lat_shift =  vsize ;
	}
	else
	{
		quads_info->north = 0 ;
		quads_info->lat_shift =  -vsize ;
	}


	if (W_ll->east > W_ll->west) 
	{
		quads_info->east = 1 ;
		quads_info->lon_shift =  hsize ;
	}
	else
	{
		quads_info->east = 0 ;
		quads_info->lon_shift =  -hsize ;
	}

	return(0) ;
}
