/**********************************************************************
 *
 *  G_make_random_colors (pcolr, min, max)
 *
 *   struct Colors *pcolr    struct to hold colors
 *   CELL min,max            min,max color numbers
 *
 *  Generates random colors that are stored in the pcolr structure. 
 *
 **********************************************************************/

#include "gis.h"

G_make_random_colors (pcolr,min,max)
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
    num = max - min + 1;

    srand((int)time((long *)0)) ;
    for(i=0; i < num; i++)
    {
	red = rand() & 0377;
	grn = rand() & 0377;
	blu = rand() & 0377;
	G_set_color ((CELL)(i+min), red, grn, blu, pcolr);
    }

    return 1;
}
