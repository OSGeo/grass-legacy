/**********************************************************************
 *
 *  G_make_color_wave (pcolr, min, max)
 *
 *   struct Colors *pcolr    struct to hold colors
 *   CELL min,max            min,max color numbers
 *
 *  Generates color wave that is stored in the pcolr structure. 
 *
 * Uses $MATHLIB routines
 **********************************************************************/

#include "gis.h"

#define PI	3.14159

G_make_color_wave (pcolr,min,max)
    struct Colors *pcolr ;
    CELL min,max;
{
    double cos() ;
    double incr ;
    double degrees ;
    double red_shift, blu_shift, grn_shift ;
    int i ;
    int num;
    int red, grn, blu;

    G_init_colors (pcolr);
    if (max < min)
	return -1;

    if (min == 1) min = 0;
    if (max == -1) max = 0;
    num = max - min + 1;

    incr = 2.0 * PI / (float)num ;
    red_shift = 0.0 ;
    blu_shift = 2. * PI / 3.0 ;
    grn_shift = blu_shift * 2. ;

    degrees = 0.0 ;
    i = 0 ;
    while(i < num)
    {
	red = ((1. + cos(degrees + red_shift)) / 2.0) * 256 ;
	grn = ((1. + cos(degrees + grn_shift)) / 2.0) * 256 ;
	blu = ((1. + cos(degrees + blu_shift)) / 2.0) * 256 ;
	G_set_color ((CELL)(i+min), red, grn, blu, pcolr);
	degrees += incr ;
	i++ ;
    }

    return 1;
}
