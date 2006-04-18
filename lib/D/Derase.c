#include <stdio.h>
#include <grass/gis.h>
#include <grass/display.h>
#include <grass/raster.h>

int Derase(char *color)
{
	int t, b, l, r ;
	int colorindex;

	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("getting graphics window") ;

	if (D_clear_window())
		G_fatal_error("clearing current graphics window") ;


	/* Parse and select background color */
	colorindex = D_parse_color (color, 0) ;

	D_raster_use_color(colorindex);

	/* Do the plotting */
	R_box_abs (l, t, r, b);

	/* Add erase item to the pad */
	D_set_erase_color(color);

	return 0;
}
