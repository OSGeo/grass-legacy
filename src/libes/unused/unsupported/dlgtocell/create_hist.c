
/*
*  Create a history file.  Some of the dlg file information  is placed in
*  the hist file.
*  returns  0  -  successful creation of history file
*           1  -  history file exists
*          -1  -  error
*/

#include "gis.h"
#include "dlg.h"

create_hist(map, dlg_struct)
	char *map ;
	struct dlg *dlg_struct ;
{
	int   fd ;
	char  *mapset ;
	char  *map_name ;
	char  *binary ;
	char  *rindex() ;
	struct History hist ;

	if ( ! (map_name = rindex(map, '/')))
		map_name = map ;
	else
		++map_name ;

	if( map_name == NULL)
		return(-1) ;

	mapset = G_mapset() ;

    /*  if we can open a hist file we don't need to create one */

	if ( (fd = G_open_old( "hist", map_name, mapset)) > 0 )
	{
		close(fd) ;
		return(1) ;

	}

	G_zero (&hist, sizeof (struct History));
	G_shorthist( mapset, "cell", &hist) ;
	strcpy(hist.title, map_name) ;

/*  store information from dlg into history  */
	sprintf(hist.datsrc_1, "  Original Scale from Dlg Map: 1:%s",
		dlg_struct->head.orig_scale) ;


	/***  copying to the  second page of history instead of 1st page
	sprintf(hist.edhist[hist.edlinecnt++], "Original Map Scale: 1:%s",
		dlg_struct->head.orig_scale) ;
	***/

	return (G_put_hist( map_name, &hist)) ;

}
