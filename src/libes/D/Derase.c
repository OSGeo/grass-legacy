#include <stdio.h>
#include "gis.h"
#include "display.h"
#include "raster.h"
#include "colors.h"

int Derase(char *color)
{
	int t, b, l, r ;
	int R, G, B, validcolor = 0;
	const int customcolor = MAXCOLORS + 1;

	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("getting graphics window") ;

	if (D_clear_window())
		G_fatal_error("clearing current graphics window") ;


	/* Parse and select background color */
	if(sscanf(color, "%d:%d:%d", &R, &G, &B) == 3) {
		if (R>=0 && R<256 && G>=0 && G<256 && B>=0 && B<256) {
			R_reset_color(R, G, B, customcolor);
			R_color(customcolor);
			validcolor=1;
		}
	}
	else {
		validcolor = D_translate_color(color);
		R_standard_color(validcolor);
	}
	if(!validcolor)
		G_fatal_error("[%s]: No such color", color);

	/* Do the plotting */
	R_box_abs (l, t, r, b);

	/* Add erase item to the pad */
	D_set_erase_color(color);

	return 0;
}
