#include "gis.h"
G_make_ryg_colors (colors, min, max)
    struct Colors *colors ;
    CELL min,max;
{
    G_init_colors (colors);
    return G_add_ryg_colors (colors, min, max);
}

G_make_red_yel_grn  (colors,min,max) /* for 3.0 compatibility */
    struct Colors *colors ;
    CELL min,max;
{
    return G_make_ryg_colors(colors,min,max);
}

G_add_ryg_colors (colors, min, max)
    struct Colors *colors ;
    CELL min,max;
{
    CELL mid;

    if (max < min) return -1;
    if (min == 1) min = 0;
    if (max == -1) max = 0;

    mid = ((double)min+(double)max)/2;

    G_add_color_rule (min, 255, 0, 0, mid, 255, 255, 0, colors);
    G_add_color_rule (mid, 255, 255, 0, max, 0, 255, 0, colors);
    G_add_color_rule ((CELL)0, 255,255,255, (CELL)0, 255,255,255, colors);

    return 1;
}
