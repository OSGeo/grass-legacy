#include "globals.h"

gen_colors ()
{
    struct Colors colr;
    int r,g,b;
    CELL x;

    G_init_colors (&colr);
    x = 0;
    for (r = 0; r < r_level; r++)
	for (g = 0; g < g_level; g++)
	    for (b = 0; b < b_level; b++)
		G_set_color (++x,
		    r*256/(r_level-1), g*256/(g_level-1), b*256/(b_level-1),
		    &colr);
    G_write_colors (result, G_mapset(), &colr);
}
