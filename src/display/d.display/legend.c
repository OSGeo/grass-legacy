#include <stdio.h>
#include "lproto.h"
#include "windows.h"
#include "raster.h"
#include "display.h"
#include "D.h"
#include "gis.h"
#include "variables.h"
#include "popup.h"

int legend(void)
{
	struct Range range ;
	struct Categories cats;
	char buff[256] ;
	int answer ;
	int cats_num ;
	int background_color ;
	int text_color ;
	int div_color ;
	int fp;
	static char *options[] = {
		" LEGEND MENU",
		"legend with text",
		"legend without text",
		"RETURN",
		NULL } ;

	background_color = D_translate_color(BC_LEG) ;
	text_color       = D_translate_color(TC_LEG) ;
	div_color        = D_translate_color(DC_LEG) ;

	if (*mapname == '\0')
	{
		fprintf (stdout,"You must draw a raster map before using this option\n") ;
		do_pause() ;
		return 0;
	}

	fp = G_raster_map_is_fp(mapname, mapset);

        if(!fp)
	{
	    if (-1 == G_read_range(mapname, mapset, &range))
	    {
		fprintf (stdout,"Error in reading category range\n") ;
		do_pause() ;
		return 0;
	    }

	    cats_num = range.max - range.min + 2 ;
	    /* one for no-data */
	}
	else
	{
	    if (-1 == G_read_raster_cats(mapname, mapset, &cats))
	    {
		fprintf (stdout,"Error in reading category file\n") ;
		do_pause() ;
		return 0;
	    }

	    cats_num = G_number_of_raster_cats(&cats) + 1 ;
        }
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
		case 1: case 2:
			if (R_open_driver() != 0)
			    G_fatal_error ("No graphics device selected");
			Dchoose(LEG.name) ;
			Derase("black") ;
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
				sprintf(buff,"'%s@%s' lines=40", mapname, mapset) ;
			else
				sprintf(buff,"'%s@%s' lines=%d", mapname, mapset, cats_num) ;
			gorun("d.legend", buff) ;
			break ;
		case 2:
			if (!fp && cats_num < 12 * 25)
				sprintf(buff,"'%s@%s' lines=25 cols=12", mapname, mapset) ;
			else
				sprintf(buff,"'%s@%s'", mapname, mapset) ;
			gorun("d.colortable", buff) ;
			break ;
		default:
			break ;
		}
	}

	return 0;
}
