/**********************************************************************
 *
 *  G_make_grey_scale_colors (colors, min, max)
 *
 *   struct Colors *colors    struct to hold colors
 *   CELL min,max             min,max color numbers
 *
 *  Generates grey scale colors that are stored in the colors structure. 
 *
 **********************************************************************/

#include "gis.h"

static int add_grey_scale_colors (struct Colors *, DCELL, DCELL);


/*!
 * \brief make linear grey scale
 *
 * Generates a grey scale color table. Each color
 * is a level of grey, increasing from black to white.
 *
 *  \param colors
 *  \param min
 *  \param max
 *  \return int
 */

int 
G_make_grey_scale_colors (struct Colors *colors, CELL min, CELL max)
{
    G_init_colors (colors);
    return add_grey_scale_colors (colors,(DCELL)min,(DCELL)max);
}

int 
G_make_grey_scale_fp_colors (struct Colors *colors, double min, double max)
{
    G_init_colors (colors);
    return add_grey_scale_colors (colors,min,max);
}

int 
G_make_grey_scale ( /* for 3.0 compatibility */
    struct Colors *colors,
    CELL min,
    CELL max
)
{
    return G_make_grey_scale_colors(colors,min,max);
}

int 
G_add_grey_scale_colors (struct Colors *colors, CELL min, CELL max)
{
    return add_grey_scale_colors (colors,(DCELL)min,(DCELL)max);
}

static int add_grey_scale_colors (struct Colors *colors, DCELL min, DCELL max)
{
    if(min > max) return -1;
    if (min == 1.) min = 0.;
    if (max == -1.) max = 0.;
    G_add_d_raster_color_rule (&min,0,0,0, &max,255,255,255, colors) ;
    return 1;
}
