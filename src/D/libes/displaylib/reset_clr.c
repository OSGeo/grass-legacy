#include "gis.h"
D_reset_colors (colors)
    struct Colors *colors;
{
    if (colors->red)
	R_reset_colors ((int)colors->min, (int)colors->max,
		colors->red, colors->grn, colors->blu);
    R_reset_colors (0, 0, &colors->r0, &colors->g0, &colors->b0);
}
