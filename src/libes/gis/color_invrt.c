#include "gis.h"

G_invert_colors (colors)
    struct Colors *colors;
{
    colors->invert = !colors->invert;
}
