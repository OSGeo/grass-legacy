#include "gis.h"

static struct
{
    int red, grn, blu;
} rules[] =
{
  255, 255,   0,       /* yellow */
    0, 255,   0,       /* green */
    0, 255, 255,       /* cyan */
    0,   0, 255,       /* blue */
  255,   0, 255,       /* magenta */
  255,   0,   0        /* red */
};

G_make_rainbow_colors (colors, min, max)
    struct Colors *colors;
    CELL min, max;
{
    G_init_colors (colors);
    return G_add_rainbow_colors (colors, min, max);
}

G_add_rainbow_colors (colors, min, max)
    struct Colors *colors;
    CELL min, max;
{
    double incr;
    int i,n;
    CELL cat1,cat2;

    if (min > max) return -1;


/* to generate the rainbow start with yellow (R & G)
 * decrease red (G)
 * then increase blue (G & B)
 * then decrease green (B)
 * then increase red (R & B)
 */
    n = sizeof(rules)/sizeof(*rules);
    incr = (double) (max-min+1)/(double)(n-1);

    cat1 = min;
    for (i = 1; i < n; i++)
    {
	if (i == n-1)
	    cat2 = max;
	else
	    cat2 = min + incr*i;

	G_add_color_rule (
		cat1, rules[i-1].red, rules[i-1].grn, rules[i-1].blu,
		cat2, rules[i].red, rules[i].grn, rules[i].blu,
		colors);
	cat1 = cat2;
    }
    return 1;
}
