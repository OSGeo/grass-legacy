#include "gis.h"
#include "display.h"
#include "raster.h"

int init_rgb (void)
{
	int offset ;
	char window_name[64] ;
	int t, b, l, r ;

	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current graphics window") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current graphics window not available") ;

	D_set_cell_name("his result") ;

/* Get color offset value for current graphics window and pass to driver */
	D_offset_is(&offset) ;
	R_color_offset(offset) ;


/* Prepare the screen window */
/*
	R_standard_color(D_translate_color("black")) ;
	D_erase_window() ;
*/
 
/* Prepare the raster cell drawing functions */
	D_get_screen_window(&t, &b, &l, &r) ;
	D_cell_draw_setup(t, b, l, r) ;

	return 0;
}
