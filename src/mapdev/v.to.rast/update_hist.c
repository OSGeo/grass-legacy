
/*
*  Update a history file.  Some of the digit file information  is placed in
*  the hist file.
*  returns  0  -  successful creation of history file
*          -1  -  error
*/

#include <stdio.h>
#include "gis.h"

update_hist(map, scale)
	char *map ;
	long scale;
{
	char  *mapset ;
	char  *map_name ;
	char  *rindex() ;
	struct History hist ;

	if ( ! (map_name = rindex(map, '/')))
		map_name = map ;
	else
		++map_name ;

	if( map_name == NULL)
		return(-1) ;

	mapset = G_mapset() ;

	if (0 > G_read_history( map_name, mapset, &hist) )
	{
		fprintf( stderr, "Couldn't read history file.\n") ;
		return(-1) ;
	}


	strcpy(hist.title, map_name) ;

/*  store information from digit file into history  */
	sprintf(hist.datsrc_1, "  Original Scale from Vector Map: 1:%ld",
		scale) ;  /* 4.0 */

	/***  copying to the  second page of history instead of 1st page
	sprintf(hist.edhist[hist.edlinecnt++], "Original Map Scale: 1:%s",
		dlg_struct->head.orig_scale) ;
	***/

	return (G_write_history( map_name, &hist)) ;

}
