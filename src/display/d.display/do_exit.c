#include "popup.h"
#include <stdio.h>

do_exit()
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
	R_open_driver();
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
}
