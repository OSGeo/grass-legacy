
/*
*  Deduces what lat, long quad we are in.
*  Written by GRASS, Fall of 88,  Michael H.
*/

#include	<stdio.h>
/****
#include	"gis.h"
****/
#include	"quad.h"


init_quad_struct (quads_info, lat_str, lon_str )
	struct  quads_description  *quads_info ;
	char  *lat_str, *lon_str ;
	
{
	double  lat_sec, lon_sec ;

/*  Get the lat/lon from the strings  */
	if ( ! CC_lat_scan( lat_str, &lat_sec) )
		return (1);
	if ( ! CC_lon_scan( lon_str, &lon_sec) )
		return (1);

/*  store it for later use  */
	quads_info->origin_lat = lat_sec ;
	quads_info->origin_lon = lon_sec ;

/*  latitude is negative in the south and positive in the north.  */
	if (lat_sec > 0.0) 
	{
		/*  NORTH  */
		quads_info->north = 1 ;
		quads_info->lat_shift =  QUAD_SIZE_SEC ;
	}
	else
	{
		quads_info->north = 0 ;
		quads_info->lat_shift =  -QUAD_SIZE_SEC ;
	}


/*  lon is negative in the east and positive in the west.  */
	if (lon_sec < 0.0) 
	{
		/*  EAST  */
		quads_info->east = 1 ;
		quads_info->lon_shift =  QUAD_SIZE_SEC ;
	}
	else
	{
		quads_info->east = 0 ;
		quads_info->lon_shift =  -QUAD_SIZE_SEC ;
	}


	return(0) ;

}

