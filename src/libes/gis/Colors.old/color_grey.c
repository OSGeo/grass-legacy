/**********************************************************************
 *
 *  G_make_grey_scale (pcolr, min, max)
 *
 *   struct Colors *pcolr    struct to hold colors
 *   CELL min,max            min,max color numbers
 *
 *  Generates grey scale colors that are stored in the pcolr structure. 
 *
 **********************************************************************/

#include "gis.h"

G_make_grey_scale (pcolr,min,max)
    struct Colors *pcolr ;
    CELL min,max;
{
    int i ;
    int num;
    int red, grn, blu;

    G_init_colors (pcolr);
    if (max < min)
	return -1;

    if (min == 1) min = 0;
    if (max == -1) max = 0;
    num = max - min;

    if (num == 0)
	G_set_color (min, 256/2, 256/2, 256/2, pcolr);
    else
	for(i=0; i <= num; i++)
	{
	    red = grn = blu = (i / (float)num) * 256 ;
	    G_set_color (min++, red, grn, blu, pcolr);
	}
    return 1;
}
