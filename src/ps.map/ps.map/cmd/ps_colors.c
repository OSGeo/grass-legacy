/* Functions: get_color_number, get_color_rgb, color_name_is_ok, get_color_name
**
** Author: Paul W. Carlson	March 1992
*/

#include <stdio.h>
#include "ps_info.h"

#define	NUM_COLORS	16

static struct {
    char *name;
    float r, g, b;
} colors[NUM_COLORS] =
{
    "white",	1.00, 1.00, 1.00,
    "black",	0.00, 0.00, 0.00,
    "red",	1.00, 0.00, 0.00,
    "green",	0.00, 1.00, 0.00,
    "blue",	0.00, 0.00, 1.00,
    "yellow",	1.00, 1.00, 0.00,
    "magenta",	1.00, 0.00, 1.00,
    "cyan",	0.00, 1.00, 1.00,
    "aqua",     0.00, 0.75, 0.75,
    "grey",	0.75, 0.75, 0.75,
    "gray",	0.75, 0.75, 0.75,
    "orange",	1.00, 0.50, 0.00,
    "brown",	0.75, 0.50, 0.25,
    "purple",	0.50, 0.00, 1.00,
    "violet",	0.50, 0.00, 1.00,
    "indigo",   0.00, 0.50, 1.00,
};

get_color_number(color_name)
char *color_name;
{
    int i;

    G_strip(color_name);
    lowercase(color_name);
    for (i = 0; i < NUM_COLORS; i++)
	if (strcmp(color_name, colors[i].name) == 0) return i;
    return -1;
}

get_color_rgb(color_number, r, g, b)
int color_number;
float *r, *g, *b;
{
    if (color_number < 0 || color_number >= NUM_COLORS)
    {
    	*r = *g = *b = 0.0;
	return -1;
    }
    *r = colors[color_number].r;
    *g = colors[color_number].g;
    *b = colors[color_number].b;
    return 1;
}

color_name_is_ok(color_name)
char *color_name;
{
    int i;

    G_strip(color_name);
    lowercase(color_name);
    for (i = 0; i < NUM_COLORS; i++)
	if (strcmp(color_name, colors[i].name) == 0) return 1;
    return 0;
}

char *get_color_name(color_number)
int color_number;
{
    int i;

    if (color_number < 0 || color_number >= NUM_COLORS) return (char *)NULL;
    return colors[color_number].name;
}

set_rgb_color(color_number)
int color_number;
{
    float r, g, b;

    if (get_color_rgb(color_number, &r, &g, &b) < 0)
    {
	r = g = b = 0.0;
    }
    fprintf(PS.fp, "%.2f %.2f %.2f C\n", r, g, b);
}
