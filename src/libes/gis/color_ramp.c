/**********************************************************************
 *
 *  G_make_ramp_colors (colors, min, max)
 *
 *   struct Colors *colors   struct to hold colors
 *   CELL min,max            min,max color numbers
 *
 *  Generates color ramp that is stored in the colors structure. 
 *
 **********************************************************************/

#include "gis.h"

G_make_ramp_colors  (colors,min,max)
    struct Colors *colors ;
    CELL min,max;
{
    G_init_colors (colors);
    return G_add_ramp_colors  (colors,min,max) ;
}

G_make_color_ramp  (colors,min,max) /* for 3.0 compatibility */
    struct Colors *colors ;
    CELL min,max;
{
    return G_make_ramp_colors(colors,min,max);
}

G_add_ramp_colors  (colors,min,max)
    struct Colors *colors ;
    CELL min,max;
{
    CELL blu1, blu2, grn1, grn2, red1, red2;
    double delta;

    if (max < min)
	return -1;

    if (min == 1) min = 0;
    if (max == -1) max = 0;
    delta = (double)max - (double)min;

    red1 = min;
    red2 = min + delta/3;
    grn1 = red2 + 1;
    grn2 = min + 2*delta/3;
    blu1 = grn2 + 1;
    blu2 = max;

    if (red1 <= red2)
	G_add_color_rule (red1, 0, 0, 0, red2, 0, 0, 255, colors);
    if (grn1 <= grn2)
	G_add_color_rule (grn1, 0, 0, 0, grn2, 0, 255, 0, colors);
    if (blu1 <= blu2)
	G_add_color_rule (blu1, 0, 0, 0, blu2, 255, 0, 0, colors);

    return 1;
}
