#include "gis.h"

static int add_ryg_colors (struct Colors *, DCELL, DCELL);

int G_make_ryg_colors (struct Colors *colors, CELL min, CELL max)
{
    G_init_colors (colors);
    return add_ryg_colors (colors, (DCELL) min, (DCELL) max);
}

int 
G_make_ryg_fp_colors (struct Colors *colors, DCELL min, DCELL max)
{
    G_init_colors (colors);
    return add_ryg_colors (colors, min, max);
}

int 
G_make_red_yel_grn ( /* for 3.0 compatibility */
    struct Colors *colors,
    CELL min,
    CELL max
)
{
    return G_make_ryg_colors(colors,min,max);
}

int 
G_add_ryg_colors (struct Colors *colors, CELL min, CELL max)
{
    return add_ryg_colors (colors, (DCELL) min, (DCELL) max);
}

static int add_ryg_colors (struct Colors *colors, DCELL min, DCELL max)
{
    DCELL mid;

    if (max < min) return -1;
    if (min == 1.) min = 0.;
    if (max == -1.) max = 0.;

    mid = (min+max)/2.;

    G_add_d_raster_color_rule (&min, 255, 0, 0, &mid, 255, 255, 0, colors);
    G_add_d_raster_color_rule (&mid, 255, 255, 0, &max, 0, 255, 0, colors);

    return 1;
}
