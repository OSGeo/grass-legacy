/**********************************************************************
 *
 *  G_make_rainbow_colors (pcolr, min, max)
 *
 *   struct Colors *pcolr    struct to hold colors
 *   CELL min,max            min,max color numbers
 *
 *  Generates color rainbow that is stored in the pcolr structure. 
 *
 **********************************************************************/

#include "gis.h"

G_make_rainbow_colors  (pcolr,min,max)
    struct Colors *pcolr ;
    CELL min,max;
{
    double x, incr;
    int i ;
    CELL cat;
    int num;

    G_init_colors (pcolr);
    if (max < min)
	return -1;

    num = max - min + 1;
    incr = 5.0 / num ;

/* to generate the rainbow start with yellow (R & G)
 * decrease red (G)
 * then increase blue (G & B)
 * then decrease green (B)
 * then increase red (R & B)
 */
    cat = min;
    for (x = 1.0; cat <= max && x >= 0.0; x -= incr) /* decrease red */
    {
	i = x * 256;
	G_set_color (cat++, i, 255, 0, pcolr);
    }
    for (x += incr; cat <= max && x <= 1.0; x += incr) /* increase blue */
    {
	i = x * 256;
	G_set_color (cat++, 0, 255, i, pcolr);
    }
    for (x -= incr; cat <= max && x >= 0.0; x -= incr) /* decrease green */
    {
	i = x * 256;
	G_set_color (cat++, 0, i, 255, pcolr);
    }
    for (x += incr; cat <= max && x <= 1.0; x += incr) /* increase red */
    {
	i = x * 256;
	G_set_color (cat++, i, 0, 255, pcolr);
    }
    for (x -= incr; cat <= max && x >= 0.0; x -= incr) /* decrease blue */
    {
	i = x * 256;
	G_set_color (cat++, 255, 0, i, pcolr);
    }
    while (cat <= max)
	G_set_color (cat++, 255, 0, 0, pcolr);

    if (min >= 0  || max <= 0)
	G_set_color ((CELL) 0, 0, 0, 0, pcolr);

    return 1;
}
