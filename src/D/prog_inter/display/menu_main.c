#define MAIN
#include "windows.h"
#include "popup.h"
#include "variables.h"
#include <stdio.h>

main()
{
	int answer ;
	int background_color ;
	int text_color ;
	int div_color ;
	static char *options[] = {
		"DISPLAY MAIN MENU",
		" map display",
		" digitize",
		" window",
		/*
		" information",
		*/
		" hide menu for 10 seconds",
		" QUIT DISPLAY",
		NULL } ;

	background_color = D_translate_color(BC_MAIN) ;
	text_color       = D_translate_color(TC_MAIN) ;
	div_color        = D_translate_color(DC_MAIN) ;

	G_gisinit("new display") ;

	setup() ;
	mapset = NULL;

/* Set the font to quick and simple */
	R_open_driver();
	R_font("romans") ;
	R_close_driver();

	for(;;)
	{
		R_open_driver();
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
			menu_display() ;
			break ;
		case 2:
			digitize() ;
			break ;
		case 3:
			window() ;
			break ;
		/*
		case 4:
			information() ;
			break ;
		*/
		case 4:
			sleep(10) ;
			break ;
		case 5:
			do_exit() ;
			break ;
		default:
			break ;
		}
	}
}
