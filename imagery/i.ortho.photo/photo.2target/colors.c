#include <grass/gis.h>
#include <grass/display.h>

int set_colors (struct Colors *colors)
{
    /* suppress color reset warnings  --  this warning ussally occurs in 
       colormode float w/ lots of colors displayed */
    G_suppress_warnings(1);
    D_set_colors (colors);
    G_suppress_warnings(0);

    return 0;
}
