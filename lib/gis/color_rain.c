#include "gis.h"

static struct
{
    int red, grn, blu;
} rules[] =
{
  {255, 255,   0},       /* yellow */
    {0, 255,   0},       /* green */
    {0, 255, 255},       /* cyan */
    {0,   0, 255},       /* blue */
  {255,   0, 255},       /* magenta */
  {255,   0,   0}        /* red */
};

static int add_rainbow_colors (struct Colors *, DCELL, DCELL);

int 
G_make_rainbow_colors (struct Colors *colors, CELL min, CELL max)
{
    G_init_colors (colors);
    return add_rainbow_colors (colors, (DCELL) min, (DCELL) max);
}

int 
G_make_rainbow_fp_colors (struct Colors *colors, DCELL min, DCELL max)
{
    G_init_colors (colors);
    G_mark_colors_as_fp(colors);
    return add_rainbow_colors (colors, min, max);
}

int 
G_add_rainbow_colors (struct Colors *colors, CELL min, CELL max)
{
    return add_rainbow_colors(colors, (DCELL) min, (DCELL) max);
}

static int add_rainbow_colors (struct Colors *colors, DCELL min, DCELL max)
{
    double incr;
    int i,n;
    DCELL val1, val2;

    if (min > max) return -1;


/* to generate the rainbow start with yellow (R & G)
 * decrease red (G)
 * then increase blue (G & B)
 * then decrease green (B)
 * then increase red (R & B)
 */
    n = sizeof(rules)/sizeof(*rules);
    incr = (max-min)/(double)(n-1);

    val1 = min;
    for (i = 1; i < n; i++)
    {
	if (i == n-1)
	    val2 = max;
	else
	    val2 = min + incr*i;

	G_add_d_raster_color_rule (
		&val1, rules[i-1].red, rules[i-1].grn, rules[i-1].blu,
		&val2, rules[i].red, rules[i].grn, rules[i].blu,
		colors);
        val1 = val2;
    }
    return 1;
}
