/**********************************************************************
 *
 *  G_make_color_ramp (pcolr, min, max)
 *
 *   struct Colors *pcolr    struct to hold colors
 *   CELL min,max            min,max color numbers
 *
 *  Generates color ramp that is stored in the pcolr structure. 
 *
 **********************************************************************/

#include "gis.h"

G_make_color_ramp  (pcolr,min,max)
    struct Colors *pcolr ;
    CELL min,max;
{
    float incr ;
    int i ;
    int red_end, grn_end, blu_end ;
    float intens ;
    int num;
    int red, grn, blu;

    G_init_colors (pcolr);
    if (max < min)
	return -1;

    if (min == 1) min = 0;
    if (max == -1) max = 0;
    num = max - min + 1;

    blu_end = num / 3 ;
    grn_end = 2 * blu_end ;
    red_end = num-1 ;
    incr = 3.0 / (float)num;

    intens = 0.0 ;
    i = 0 ;
    while(i<=blu_end)
    {
	intens += incr ;
	intens = (intens < 1.0) ? intens : 1.0 ;

	blu = intens * 256;
	grn = 0;
	red = 0;

	G_set_color ((CELL)(i+min), red, grn, blu, pcolr);
	i++ ;
    }
    intens = 0.0 ;
    while(i<=grn_end)
    {
	intens += incr ;
	intens = (intens < 1.0) ? intens : 1.0 ;

	grn = intens * 256;
	blu = 0;
	red = 0;

	G_set_color ((CELL)(i+min), red, grn, blu, pcolr);
	i++ ;
    }
    intens = 0.0 ;
    while(i<=red_end)
    {
	intens += incr ;
	intens = (intens < 1.0) ? intens : 1.0 ;

	red = intens * 256;
	blu = 0;
	grn = 0;

	G_set_color ((CELL)(i+min), red, grn, blu, pcolr);
	i++ ;
    }
    return 1;
}
