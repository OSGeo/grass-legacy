#include "popup.h"
#include "lproto.h"
#include "display.h"
#include "raster.h"
#include "gis.h"
#include <stdio.h>

int do_exit()
{
	int answer ;
	int background_color ;
	int text_color ;
	int div_color;
	static char *options[] = {
		"REALLY QUIT?",
		"YES",
		"NO",
		NULL } ;

	background_color = D_translate_color(BC_EXIT) ;
	text_color       = D_translate_color(TC_EXIT) ;
	div_color        = D_translate_color(DC_EXIT) ;

	tell_em_to_use_mouse() ;
	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");
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

	if (answer == 1)
		exit(0) ;

	return 0;
}
