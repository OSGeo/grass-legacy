#include "gis.h"
G_set_color_range (min, max, colors)
    CELL min, max;
    struct Colors *colors;
{
    if (min < max)
    {
	colors->cmin = min;
	colors->cmax = max;
    }
    else
    {
	colors->cmin = max;
	colors->cmax = min;
    }
}

G_get_color_range(min, max, colors)
    CELL *min, *max;
    struct Colors *colors;
{
    *min = colors->cmin;
    *max = colors->cmax;
}
