/**********************************************************************
 *
 *  G_make_aspect_colors (colors, min, max)
 *
 *   struct Colors *colors    struct to hold colors
 *   CELL min,max             min,max color numbers
 *
 *  Generates aspect colors that are stored in the colors structure. 
 *
 **********************************************************************/

#include "gis.h"
static int add_aspect_colors (struct Colors *, DCELL, DCELL);


/*!
 * \brief make aspect colors
 *
 * Generates a color table for aspect data.
 *
 *  \param colors
 *  \param min
 *  \param max
 *  \return int
 */

int G_make_aspect_colors (struct Colors *colors, CELL min, CELL max)
{
    G_init_colors (colors);
    return add_aspect_colors (colors,(DCELL) min,(DCELL) max) ;
}

int G_make_aspect_fp_colors (struct Colors *colors, DCELL min, DCELL max)
{
    G_init_colors (colors);
    return add_aspect_colors (colors,min,max) ;
}

int G_add_aspect_colors (struct Colors *colors, CELL min, CELL max)
{
    return add_aspect_colors (colors,(DCELL) min,(DCELL) max) ;
}

static int add_aspect_colors (struct Colors *colors, DCELL min, DCELL max)
{
    DCELL half;

    if (max < min)
	return -1;

    if (min == 1.) min = 0.;
    if (max == -1.) max = 0.;
    half = (min+max)/2.;
 
    G_add_d_raster_color_rule (&min, 0,0,0, &half, 255, 255, 255, colors);
    G_add_d_raster_color_rule (&half, 255, 255, 255, &max, 0,0,0, colors);

    return 1;
}
