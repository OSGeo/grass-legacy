#include "gis.h"

int toggle_number = 0 ;

table_toggle(name, mapset, colors)
    char *name ;
    char *mapset ;
    struct Colors *colors ;
{
    CELL min, max;

    G_get_color_range (&min, &max, colors);
    G_free_colors(colors);

    toggle_number = ++toggle_number % 6 ;
    switch(toggle_number & 0177)
    {
        case 0:
            G_read_colors(name, mapset, colors) ;
            break ;
        case 1:
            G_make_ramp_colors(colors, min, max) ;
            break ;
        case 2:
            G_make_grey_scale_colors(colors, min, max) ;
            break ;
        case 3:
            G_make_random_colors(colors, min, max) ;
            break ;
        case 4:
            G_make_wave_colors(colors, min, max) ;
            break ;
        case 5:
            G_make_aspect_colors(colors, min, max) ;
            break ;
    }
}
