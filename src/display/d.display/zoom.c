#include "gis.h"
#include "lproto.h"
#include "raster.h"
#include "display.h"
#include "D.h"
#include "windows.h"
#include "variables.h"
#include "popup.h"

int zoom()
{
	struct Cell_head wind ;
	int i ;
	int background_color ;
	int text_color ;
	int div_color ;
	static char *options[] = {
		"REGION ZOOM",
		" zoom in",
		" zoom out",
		" type coordinates",
		" RETURN",
		NULL } ;

	background_color = D_translate_color(BC_WIND) ;
	text_color       = D_translate_color(TC_WIND) ;
	div_color        = D_translate_color(DC_WIND) ;

	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");
	tell_em_to_use_mouse() ;
	i = D_popup(
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

	switch(i)
	{
	case 1:
		if (R_open_driver() != 0)
			G_fatal_error ("No graphics device selected");
		Dchoose(MAP.name) ;
		R_close_driver();
		G_clear_screen() ;
		fprintf (stdout,"\n\n   Please identify new region on main map\n") ;
	/* d.zoom exits with 0 if the region is changed */
		if (gorun("d.zoom", "-q"))
			return 1;
		break ;
	case 2:
		if (R_open_driver() != 0)
			G_fatal_error ("No graphics device selected");
		Dchoose(REF.name) ;
		R_close_driver();
		G_clear_screen() ;
		fprintf (stdout,"\n\n   Please identify new region on\n") ;
		fprintf (stdout,"   small reference map at upper right\n") ;
	/* d.zoom exits with 0 if the region is changed */
		if (gorun("d.zoom", "-q"))
			return 1;
		break ;
	case 3:
		gorun("g.region", "") ;
		break ;
	default:
		return 0;
		break ;
	}


	show_region() ;

	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");

/* Draw cell map */
	/* G__get_window called here because we need to read the current window
	 * which may have changed.  G_get_window reads the window only once.
	 */
	G__get_window (&wind,"","WIND",G_mapset()) ;
	G_set_window(&wind) ;
	Dchoose(MAP.name) ;
	Derase("black") ;
	if (mapset)
	    Dcell(mapname, mapset, 0) ;

/* Draw tiny map */
	G_get_default_window(&wind) ;
	G_set_window(&wind) ;
	Dchoose(REF.name) ;
	if (mapset)
	    Dcell(mapname, mapset, 0) ;

	R_close_driver();

/* Draw outline of current window on tiny map */
	Draw_outline() ;

	return 0;
}
