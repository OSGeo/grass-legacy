#include "gis.h"
#include "display.h"
#include "raster.h"

int Derase(char *color)
{
	int t, b, l, r ;

	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("getting graphics window") ;

	if (D_clear_window())
		G_fatal_error("clearing current graphics window") ;

	/* Do the plotting */
	R_standard_color(D_translate_color(color)) ;
	R_box_abs (l, t, r, b);

	/* Add erase item to the pad */
	D_set_erase_color(color);

	return 0;
}
