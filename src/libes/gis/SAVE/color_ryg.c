#include "gis.h"
G_make_red_yel_grn (pcolr, min, max)
    struct Colors *pcolr ;
    CELL min,max;
{
    int i, j ;
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

    red = 256 ;  blu = 0 ;
    for(i=1; i<=n; i++)
    {
	grn = ((float)i / (float)n) * 256 ;
	G_set_color ((CELL)(i+min), red, grn, blu, pcolr);
    }

    grn = 256 ;  blu = 0 ;
    j=0 ;
    for(; i<num; i++)
    {
	red = 256 - ((float)(j++) / (float)n) * 256 ;
	G_set_color ((CELL)(i+min), red, grn, blu, pcolr);
    }
    G_set_color ((CELL)(0), 256, 256, 256, pcolr);
    return 1;
}
