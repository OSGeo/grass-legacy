#include <string.h>
#include "gis.h"
#include "display.h"
#include "raster.h"

int Dremove(char *new_wind)
{
	char cur_wind[64] ;
	int i ;
	int t, b, r, l, line ;

	if (D_get_cur_wind(cur_wind))
		return(-1) ;
	
	if (! strcmp(new_wind, cur_wind))
		return(-1) ;

	if(i = D_set_cur_wind(new_wind))
		return(i) ;

	if (D_get_screen_window(&t, &b, &l, &r))
		return(-1) ;

/* Do the plotting */
	R_standard_color(D_translate_color(DEFAULT_BG_COLOR)) ;
	for(line=t-1; line<b+2; line++)
	{
		R_move_abs(l-1, line) ;
		R_cont_abs(r+1, line) ;
	}

	if (D_remove_window())
		G_fatal_error("Remove window") ;

/* Reset current window */
	D_set_cur_wind(cur_wind) ;

	return(0) ;
}
