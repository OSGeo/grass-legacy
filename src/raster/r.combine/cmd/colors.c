#include "gis.h"
#include "colors.h"
make_16_colors(color)
    struct Colors *color ;
{
    CELL cat ;

/* make color combos */
    G_init_colors (color) ;
    for (cat=0; cat<=15; cat++)
	G_set_color (cat, (int)red[cat], (int)grn[cat], (int)blu[cat], color);

    return(0) ;
}

