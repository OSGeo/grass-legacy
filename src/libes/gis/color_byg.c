#include "gis.h"

static int add_byg_colors (struct Colors *, DCELL, DCELL);

int G_make_byg_colors (struct Colors *colors, CELL min, CELL max)
{
    G_init_colors (colors);
    return add_byg_colors (colors, (DCELL) min, (DCELL) max);
}

int 
G_make_byg_fp_colors (struct Colors *colors, DCELL min, DCELL max)
{
    G_init_colors (colors);
    return add_byg_colors (colors, min, max);
}

int 
G_make_blue_yel_grn ( /* for 3.0 compatibility */
    struct Colors *colors,
    CELL min,
    CELL max
)
{
    return G_make_byg_colors(colors,min,max);
}

int 
G_add_byg_colors (struct Colors *colors, CELL min, CELL max)
{
    return add_byg_colors (colors, (DCELL) min, (DCELL) max);
}

static int add_byg_colors (struct Colors *colors, DCELL min, DCELL max)
{
    DCELL mid;

    if (max < min) return -1;
    if (min == 1.) min = 0.;
    if (max == -1.) max = 0.;

    mid = (min+max)/2.;

    G_add_d_raster_color_rule (&min, 0, 0, 255, &mid, 255, 255, 0, colors);
    G_add_d_raster_color_rule (&mid, 255, 255, 0, &max, 0, 255, 0, colors);
    mid = 0.;
    G_add_d_raster_color_rule (&mid, 255,255,255, &mid, 255,255,255, colors);

    return 1;
}
