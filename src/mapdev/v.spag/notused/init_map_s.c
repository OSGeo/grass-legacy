

#include	<stdio.h>
#include	"gis.h"
#include	"head.h"
#include	"dig_structs.h"


init_map_struct(Map)
	struct Map_info *Map ;
{

    /*  store the map name and point to it  */
	Map->name = G_calloc ( sizeof(head.map_name), sizeof(char)) ;
	strncpy( Map->name, head.map_name, sizeof(head.map_name)) ;

	Map->id = 0 ;
	Map->n_nodes = 0 ;
	Map->n_lines = 0 ;
	Map->n_areas = 0 ;
	Map->n_points = 0 ;
	Map->alloc_nodes = 0 ;
	Map->alloc_lines = 0 ;
	Map->alloc_areas = 0 ;

    /*  Map->digit and  Map->plus already assigned in open_files()  */
	Map->att = NULL ;

	retrun(0) ;
}


