#include "variables.h"
#include "options.h"
#include "gis.h"
#include <stdio.h>

color()
{
	char buff[128] ;
	struct Cell_head window ;
	char window_name[64] ;
	int offset ;
	if (*mapname == NULL)
	{
		printf("You must draw a cell map before using this option\n") ;
		do_pause() ;
		return ;
	}
	strcpy(map_name, mapname);


/* Read in the map window associated with graphics window */
	G_get_window(&window) ;

	if (G_set_window(&window) == -1) 
		G_fatal_error("Current window not settable") ;

	R_open_driver();

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current graphics window (Run Dscreen)") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current graphics window not available") ;

/* Get color offset value for current graphics window and pass to driver */
	D_offset_is(&offset) ;
	R_color_offset(offset) ;

	get_map_info() ;

	R_close_driver();
}
