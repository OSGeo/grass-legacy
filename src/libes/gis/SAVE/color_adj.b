#include "gis.h"

/*********
 * this code is experimental and will probably change
 * both in calling sequences and routine names
 *********/

G_set_color_gamma(r,g,b,colors)
    double r,g,b;
    struct Colors *colors;
{
    init(colors);
    colors->correction.red.gamma *= r;
    colors->correction.grn.gamma *= g;
    colors->correction.blu.gamma *= b;
}

G_set_color_filter(r,g,b,colors)
    double r,g,b;
    struct Colors *colors;
{
    init(colors);
    colors->correction.red.filter += r;
    colors->correction.grn.filter += g;
    colors->correction.blu.filter += b;
}

G_set_color_lightness(r,g,b,colors)
    double r,g,b;
    struct Colors *colors;
{
    init(colors);
    colors->correction.red.lightness *= r;
    colors->correction.grn.lightness *= g;
    colors->correction.blu.lightness *= b;
}

G_reset_color_corrections(colors)
    struct Colors *colors;
{
    colors->correction.active = 0;
    colors->correction.pending = 0;
}

static
init(colors)
    struct Colors *colors;
{
    if (colors->correction.pending) return;
    colors->correction.pending = 1;
    colors->correction.active = 0;

    colors->correction.red.gamma = 1.0;
    colors->correction.red.lightness = 1.0;
    colors->correction.red.filter = 0.0;

    colors->correction.grn.gamma = 1.0;
    colors->correction.grn.lightness = 1.0;
    colors->correction.grn.filter = 0.0;

    colors->correction.blu.gamma = 1.0;
    colors->correction.blu.lightness = 1.0;
    colors->correction.blu.filter = 0.0;
}

/* see organize.c for code to create the correction tables
 * and lookup.c for code that performs the correction lookup
 */
