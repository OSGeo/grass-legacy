#define MAIN
#include "options.h"
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
		"@DISPLAY MAIN MENU",
		"@map display",
		"@digitize",
		"@window",
		"@information",
		"@hide menu for 10 seconds",
		"@QUIT",
		NULL } ;

	background_color = D_translate_color(BC_MAIN) ;
	text_color       = D_translate_color(TC_MAIN) ;
	div_color        = D_translate_color(DC_MAIN) ;

	G_gisinit("new display") ;

	setup() ;
	mapset = NULL;

	for(;;)
	{
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
		case 4:
			information() ;
			break ;
		case 5:
			sleep(10) ;
			break ;
		case 6:
			do_exit() ;
			break ;
		default:
			break ;
		}
	}
}
