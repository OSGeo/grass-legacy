/**********************************************************************
 *
 *  G_make_aspect_colors (colors, min, max)
 *
 *   struct Colors *colors    struct to hold colors
 *   CELL min,max             min,max color numbers
 *
 *  Generates aspect colors that are stored in the colors structure. 
 *
 **********************************************************************/

#include "gis.h"

G_make_aspect_colors (colors,min,max)
    struct Colors *colors ;
    CELL min,max;
{
    G_init_colors (colors);
    return G_add_aspect_colors (colors,min,max) ;
}

G_add_aspect_colors (colors,min,max)
    struct Colors *colors ;
    CELL min,max;
{
    CELL half;

    if (max < min)
	return -1;

    if (min == 1) min = 0;
    if (max == -1) max = 0;
    half = ((double)min+(double)max)/2;

    G_add_color_rule (min, 0,0,0, half, 255, 255, 255, colors);
    G_add_color_rule (half, 255, 255, 255, max, 0,0,0, colors);

    return 1;
}
