#include "gis.h"

/* for convenience, but to be avoided if possible */
int G_set_color (
    CELL cat,int r,int g, int b,
    struct Colors *colors)
{
    CELL tmp=cat;

    if(G_is_c_null_value(&tmp))
       return G_set_null_value_color(r, g, b, colors);
    return G_add_color_rule (cat, r,g,b, cat, r,g,b, colors);
}

int G_set_d_color (val, r, g, b, colors)
    DCELL val;
    struct Colors *colors;
{
    DCELL tmp=val;
    if(G_is_d_null_value(&tmp))
       return G_set_null_value_color(r, g, b, colors);
    return G_add_d_raster_color_rule (&val, r,g,b, 
				      &val, r,g,b, colors);
}

int G_set_null_value_color(red, grn, blu, colors)
   int red, blu, grn;
   struct Colors *colors;

{
        colors->null_red = red;
        colors->null_grn = grn;
        colors->null_blu = blu;
        colors->null_set = 1;
        return 1;
}

int G_set_default_color(red, grn, blu, colors)
   int red, blu, grn;
   struct Colors *colors;

{
        colors->undef_red = red;
        colors->undef_grn = grn;
        colors->undef_blu = blu;
        colors->undef_set = 1;
        return 1;
}

