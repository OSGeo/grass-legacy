#include "gis.h"

static int add_gyr_colors (struct Colors *, DCELL, DCELL);

int 
G_make_gyr_colors (struct Colors *colors, CELL min, CELL max)
{
    G_init_colors (colors);
    return add_gyr_colors (colors, (DCELL) min, (DCELL) max);
}

int 
G_make_gyr_fp_colors (struct Colors *colors, DCELL min, DCELL max)
{
    G_init_colors (colors);
    return add_gyr_colors (colors, min, max);
}

int 
G_add_gyr_colors (struct Colors *colors, CELL min, CELL max)
{
    return add_gyr_colors (colors, (DCELL) min, (DCELL) max);
}

static int add_gyr_colors (struct Colors *colors, DCELL min, DCELL max)
{
    DCELL half;

    if (max < min) return -1;
    if (min == 1.) min = 0.;
    if (max == -1.) max = 0.;

    half = (min+max)/2.;

    G_add_d_raster_color_rule (&min, 0, 255, 0, &half, 255, 255, 0, colors);
    G_add_d_raster_color_rule (&half, 255, 255, 0, &max, 255, 0, 0, colors);
    half = 0.;
    G_add_d_raster_color_rule (&half, 255,255,255, &half, 255,255,255, colors);

    return 1;
}
