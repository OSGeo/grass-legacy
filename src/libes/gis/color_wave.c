/**********************************************************************
 *
 *  G_make_wave_colors (colors, min, max)
 *
 *   struct Colors *colors   struct to hold colors
 *   CELL min,max            min,max color numbers
 *
 *  Generates color wave that is stored in the colors structure. 
 *
 **********************************************************************/

#include "gis.h"

static int add_wave_colors (struct Colors *, DCELL, DCELL);


/*!
 * \brief make color wave
 *
 * Generates a color table with 3 sections: red only,
 * green only, and blue only, each increasing from none to full intensity and
 * back down to none.  This table is good for continuous data like elevation.
 *
 *  \param colors
 *  \param min
 *  \param max
 *  \return int
 */

int 
G_make_wave_colors (struct Colors *colors, CELL min, CELL max)
{
    G_init_colors (colors);
    return add_wave_colors  (colors,min,max) ;
}

int 
G_make_wave_fp_colors (struct Colors *colors, DCELL min, DCELL max)
{
    G_init_colors (colors);
    return add_wave_colors  (colors,min,max) ;
}

int 
G_make_color_wave ( /* for 3.0 compatibility */
    struct Colors *colors,
    CELL min,
    CELL max
)
{
    return G_make_wave_colors(colors,min,max);
}

int 
G_add_wave_colors (struct Colors *colors, CELL min, CELL max)
{
    return add_wave_colors  (colors,(DCELL) min,(DCELL) max) ;
}

static int add_wave_colors (struct Colors *colors, DCELL min, DCELL max)
{
    DCELL x1,x2,x3,x4,x5;

    if (max < min)
	return -1;

    if (min == 1) min = 0;
    if (max == -1) max = 0;

    x1 = (5.0*min + 1.0*max)/6.0;
    x2 = (4.0*min + 2.0*max)/6.0;
    x3 = (3.0*min + 3.0*max)/6.0;
    x4 = (2.0*min + 4.0*max)/6.0;
    x5 = (1.0*min + 5.0*max)/6.0;

    if (min <= x1)
	G_add_d_raster_color_rule (&min, 255, 85, 85, &x1, 170, 170, 0, colors);
    if (x1 <= x2)
	G_add_d_raster_color_rule (&x1, 170, 170, 0, &x2, 85, 255, 85, colors);
    if (x2 <= x3)
	G_add_d_raster_color_rule (&x2, 85, 255, 85, &x3, 0, 170, 170, colors);
    if (x3 <= x4)
	G_add_d_raster_color_rule (&x3, 0, 170, 170, &x4, 85, 85, 255, colors);
    if (x4 <= x5)
	G_add_d_raster_color_rule (&x4, 85, 85, 255, &x5, 170, 0, 170, colors);
    if (x5 <= max)
	G_add_d_raster_color_rule (&x5, 170, 0, 170, &max, 255, 85, 85, colors);

    return 1;
}
