/**********************************************************************
 *
 *  G_make_aspect_colors (pcolr, min, max)
 *
 *   struct Colors *pcolr    struct to hold colors
 *   CELL min,max            min,max color numbers
 *
 *  Generates aspect colors that are stored in the pcolr structure. 
 *
 **********************************************************************/

#include "gis.h"

G_make_aspect_colors (pcolr,min,max)
    struct Colors *pcolr ;
    CELL min,max;
{
    int i ;
    int num ;
    int n;
    int red, grn, blu;

    G_init_colors (pcolr);
    if (max < min)
	return -1;

    if (min == 1) min = 0;
    if (max == -1) max = 0;
    num = max - min + 1;

    n = num / 2;

    for(i=0; i<=n; i++)
    {
	red = grn = blu = (.20 + .60 * (float)(2*i) / (float)num) * 256 ;
	G_set_color ((CELL)(i+min), red, grn, blu, pcolr);
	G_set_color ((CELL)(num-i+min), red, grn, blu, pcolr);
    }
    return 1;
}
