
 /*
 *  Stores the environmental variables to describe a window to
 *  the .grassrc file.
 *
 *  Written by the GRASS Team in the Spring of 90, -mh.
 *
 */

#include	<stdio.h>
#include	"gis.h"

extern  int SCREEN_LEFT ;
extern  int SCREEN_RIGHT ;
extern  int SCREEN_BOTTOM ;
extern  int SCREEN_TOP ;
extern  int SCREEN_VS ;
extern  int NCOLORS ;
extern  int IGRAPH_CLRS_SAV ;

Put_to_grass_env() 
{

put_to_grass_env ("IGRAPH_TOP" ,SCREEN_TOP );
put_to_grass_env ("IGRAPH_BOT" ,SCREEN_BOTTOM );
put_to_grass_env ("IGRAPH_RIGHT" ,SCREEN_RIGHT );
put_to_grass_env ("IGRAPH_LEFT" ,SCREEN_LEFT );
put_to_grass_env ("IGRAPH_VS" ,SCREEN_VS );
/*put_to_grass_env ("IGRAPH_COLORS" ,NCOLORS );*/
put_to_grass_env ("IGRAPH_COLORS" ,IGRAPH_CLRS_SAV );

}

static
put_to_grass_env (str , value )
	char  *str ;
	int  value ;
{
	char string_value[100] ;

	sprintf( string_value, "%d", value) ;
	G_setenv( str, string_value) ;
}
