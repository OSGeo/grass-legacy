#include "gis.h"

/* return RGB for given category */
G_get_color (n, red, grn, blu, colors)
    CELL n;
    int *red, *grn, *blu;
    struct Colors *colors;
{
    CELL cat;
    unsigned char r, g, b, set;

    cat = n;
    G_lookup_colors (&cat, &r, &g, &b, &set, 1, colors);

    *red = (int) r;
    *grn = (int) g;
    *blu = (int) b;

    return (int)set;
}
