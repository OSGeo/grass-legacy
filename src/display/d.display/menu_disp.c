#include "popup.h"
#include "lproto.h"
#include "raster.h"
#include "gis.h"
#include "display.h"
#include <stdio.h>

int menu_display()
{
	int answer ;
	int background_color ;
	int text_color ;
	int div_color ;
	static char *options[] = {
		"DISPLAY MAP MENU",
		"raster map    2-d",
		"vector map    2-d",
		/*
		"label file    2-d",
		*/
		"site file     2-d",
		"scale display 2-d",
		"measurements  2-d",
		"color interact",
		"legend",
		"raster map 3-d",
		"RETURN",
		NULL } ;

	background_color = D_translate_color(BC_DISP) ;
	text_color       = D_translate_color(TC_DISP) ;
	div_color        = D_translate_color(DC_DISP) ;

	for(;;)
	{
		if (R_open_driver() != 0)
		    G_fatal_error ("No graphics device selected");
		tell_em_to_use_mouse() ;
		answer = D_popup(
			background_color,
			text_color,
			div_color,
			TOP,
			LEFT,
			SIZE,
			options
			) ;
		G_clear_screen() ;
		R_close_driver();

		switch(answer)
		{
		case 1:
			cell_map() ;
			break ;
		case 2:
			vect_map() ;
			break ;
		/*
		case 3:
			label() ;
			break ;
		*/
		case 3:
			site() ;
			break ;
		case 4:
			scale() ;
			break ;
		case 5:
			measurements() ;
			break ;
		case 6:
			color() ;
			break ;
		case 7:
			legend() ;
			break ;
		case 8:
			cell3d() ;
			break ;
		case 9:
			return(0) ;
			break ;
		default:
			break ;
		}
	}

	return 0;
}
