#include "popup.h"
#include "windows.h"
#include <stdio.h>

do_exit()
{
	int answer ;
	int background_color ;
	int text_color ;
	int div_color;
	char buffer[40];
	static char *options[] = {
		"@REALLY QUIT?",
		"@YES",
		"@NO",
		NULL } ;

  	background_color = D_translate_color(BC_EXIT) ;
	text_color       = D_translate_color(TC_EXIT) ;
	div_color        = D_translate_color(DC_EXIT) ;

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
	R_close_driver(); 

	if (answer == 1)
	{	R_open_driver();
		Dnew("full screen", 0., 100., 0., 100.);
		Dchoose("full screen");
		R_close_driver();
		exit(0) ;
	}
}
