/**********************************************************************
 *
 *  G_make_grey_scale_colors (colors, min, max)
 *
 *   struct Colors *colors    struct to hold colors
 *   CELL min,max             min,max color numbers
 *
 *  Generates grey scale colors that are stored in the colors structure. 
 *
 **********************************************************************/

#include "gis.h"

G_make_grey_scale_colors (colors,min,max)
    struct Colors *colors ;
    CELL min,max;
{
    G_init_colors (colors);
    return G_add_grey_scale_colors (colors,min,max);
}

G_make_grey_scale  (colors,min,max) /* for 3.0 compatibility */
    struct Colors *colors ;
    CELL min,max;
{
    return G_make_grey_scale_colors(colors,min,max);
}

G_add_grey_scale_colors (colors,min,max)
    struct Colors *colors ;
    CELL min,max;
{
    if(min > max) return -1;
    if (min == 1) min = 0;
    if (max == -1) max = 0;
    G_add_color_rule (min,0,0,0, max,255,255,255, colors) ;
    return 1;
}
