#include <grass/gis.h>
#include <grass/display.h>

int set_colors (struct Colors *colors)
{
    D_set_colors (colors);

    return 0;
}
