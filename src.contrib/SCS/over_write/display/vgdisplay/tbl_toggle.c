/*  %W%  %G%  */

#include "gis.h"

int toggle_number ;

table_toggle(name, mapset, colors)
	char *name ;
	char *mapset ;
	struct Colors *colors ;
{
/*** NOTE: In the next statement, the number "6" was changed to "5" by
 ***       Paul Carlson because G_make_aspect_colors crashed the program.
 ***/
	toggle_number = ++toggle_number % 5 ;
	switch(toggle_number & 0177)
	{
		case 0:
			G_read_colors(name, mapset, colors) ;
			break ;
		case 1:
			G_make_color_ramp(colors, colors->min, colors->max) ;
			break ;
		case 2:
			G_make_grey_scale(colors, colors->min, colors->max) ;
			break ;
		case 3:
			G_make_random_colors(colors, colors->min, colors->max) ;
			break ;
		case 4:
			G_make_color_wave(colors, colors->min, colors->max) ;
			break ;
		case 5:
			G_make_aspect_colors(colors, colors->min, colors->max) ;
			break ;
	}
	R_reset_colors(colors->min, colors->max, colors->red, colors->grn, colors->blu) ;
}
