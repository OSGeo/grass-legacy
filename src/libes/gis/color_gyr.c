#include "gis.h"
G_make_gyr_colors (colors, min, max)
    struct Colors *colors ;
    CELL min,max;
{
    G_init_colors (colors);
    return G_add_gyr_colors (colors, min, max);
}

G_add_gyr_colors (colors, min, max)
    struct Colors *colors ;
    CELL min,max;
{
    CELL half;

    if (max < min) return -1;
    if (min == 1) min = 0;
    if (max == -1) max = 0;

    half = ((double)min+(double)max)/2;

    G_add_color_rule (min, 0, 255, 0, half, 255, 255, 0, colors);
    G_add_color_rule (half, 255, 255, 0, max, 255, 0, 0, colors);
    G_add_color_rule ((CELL)0, 255,255,255, (CELL)0, 255,255,255, colors);

    return 1;
}
