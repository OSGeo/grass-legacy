#include "gis.h"

#define GREY 1
#define WAVE 2

static int type = GREY;

int 
make_colors (struct Colors *colors, CELL min, CELL max)
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

    return 0;
}

int 
select_colors (char *name)
{
    if (strcmp (name, "grey") == 0 )
	type = GREY;
    else if (strcmp (name, "wave") == 0 )
	type = WAVE;
    else
	fprintf (stdout,"color type <%s> unknown.\n", name);

    switch (type)
    {
    case WAVE: fprintf (stdout,"color wave selected\n"); break;
    default:   fprintf (stdout,"grey scale selected\n"); break;
    }

    return 0;
}
