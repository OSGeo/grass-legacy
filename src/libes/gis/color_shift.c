#include "gis.h"

G_shift_colors (shift, colors)
    struct Colors *colors;
{
    colors->shift += shift;
}
