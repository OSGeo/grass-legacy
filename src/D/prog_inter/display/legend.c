#include "windows.h"
#include "gis.h"
#include "variables.h"
#include "popup.h"
#include <stdio.h>

legend()
{
	struct Range range ;
	char buff[256] ;
	int answer ;
	int cats_num ;
	int background_color ;
	int text_color ;
	int div_color ;
	static char *options[] = {
		" LEGEND MENU",
		"legend with text",
		"legend without text",
		"RETURN",
		NULL } ;

	background_color = D_translate_color(BC_LEG) ;
	text_color       = D_translate_color(TC_LEG) ;
	div_color        = D_translate_color(DC_LEG) ;

	if (*mapname == NULL)
	{
		printf("You must draw a cell map before using this option\n") ;
		do_pause() ;
		return ;
	}

	if (-1 == G_read_range(mapname, mapset, &range))
	{
		printf("Error in reading category range\n") ;
		do_pause() ;
		return ;
	}

	cats_num = range.pmax - range.nmin + 1 ;

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
		case 1: case 2:
			R_open_driver();
			Dchoose(LEG.name) ;
			Derase(D_translate_color("black")) ;
			R_close_driver();
			break ;
		default:
			break ;
		}

		switch(answer)
		{
		case 3:
			return(0) ;
			break ;
		case 1:
			if (cats_num < 40)
				sprintf(buff,"'%s in %s' lines=40", mapname, mapset) ;
			else
				sprintf(buff,"'%s in %s' lines=%d", mapname, mapset, cats_num) ;
			gorun("Dlegend", buff) ;
			break ;
		case 2:
			if (cats_num < 12 * 25)
				sprintf(buff,"'%s in %s' lines=25 cols=12", mapname, mapset) ;
			else
				sprintf(buff,"'%s in %s'", mapname, mapset) ;
			gorun("Dcolortable", buff) ;
			break ;
		default:
			break ;
		}
	}
}
