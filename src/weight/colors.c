#include "gis.h"

#define GREY 1
#define WAVE 2

static int type = GREY;

make_colors (colors, min, max)
    struct Colors *colors;
    CELL min, max;
{
    switch (type)
    {
    case WAVE:
	G_make_color_wave (colors, min, max);
	break;
    default:
	G_make_grey_scale (colors, min, max);
	break;
    }
}

select_colors (name)
    char *name;
{
    if (strcmp (name, "grey") == 0 )
	type = GREY;
    else if (strcmp (name, "wave") == 0 )
	type = WAVE;
    else
	printf ("color type <%s> unknown.\n", name);

    switch (type)
    {
    case WAVE: printf ("color wave selected\n"); break;
    default:   printf ("grey scale selected\n"); break;
    }
}
