#include "display.h"
#include "lproto.h"
#include "popup.h"
#include "raster.h"
#include "gis.h"
#include <stdio.h>

int information()
{
	int answer ;
	int background_color ;
	int text_color ;
	int div_color ;
	static char *options[] = {
		"INFORMATION MENU",
		"histogram of displayed raster map",
		"RETURN",
		NULL } ;

	background_color = D_translate_color(BC_INFO) ;
	text_color       = D_translate_color(TC_INFO) ;
	div_color        = D_translate_color(DC_INFO) ;

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
			histo_plot() ;
			break ;
		case 2:
			return(0) ;
			break ;
		default:
			break ;
		}
	}

	return 0;
}
