
/*
 *  Written by the GRASS Team in the Spring of 90, -mh.
*/

#include	"gis.h"
#include	"env.h"

store_vs (desc)
	struct  screen_description *desc  ;
{

	store_to_grass_file ("IGRAPH_VS" ,desc->vs_no );
}

store_color (desc)
	struct  screen_description *desc  ;
{
	store_to_grass_file ("IGRAPH_COLORS" ,desc->num_colors );
}

store_dimensions (desc)
	struct  screen_description *desc  ;
{

	store_to_grass_file ("IGRAPH_TOP" ,desc->top );
	store_to_grass_file ("IGRAPH_BOT" ,desc->bottom );
	store_to_grass_file ("IGRAPH_RIGHT" ,desc->right );
	store_to_grass_file ("IGRAPH_LEFT" ,desc->left );
}

 /*
 *  Stores the environmental variables to describe a window to
 *  the .grassrc file.
 *
 */


static
store_to_grass_file (str , value )
	char  *str ;
	int  value ;
{
	char string_value[100] ;

	sprintf( string_value, "%d", value) ;
	G_setenv( str, string_value) ;
}


