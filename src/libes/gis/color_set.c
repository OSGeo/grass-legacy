#include "gis.h"

/* for convenience, but to be avoided if possible */
G_set_color (cat, r, g, b, colors)
    CELL cat;
    struct Colors *colors;
{
    return G_add_color_rule (cat, r,g,b, cat, r,g,b, colors);
}
