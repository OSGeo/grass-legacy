#include "gis.h"

#define LOOKUP_COLORS 2048

static int organizing = 0;

G__organize_colors (colors)
    struct Colors *colors;
{

/* don't do anything if called recursively */
    if (!organizing)
    {
	organizing = 1;

	organize_lookup (colors, 0);
	organize_lookup (colors, 1);

	organizing = 0;
    }
}

static
organize_lookup(colors, mod)
    struct Colors *colors;
{
    int i, n ;
    CELL x;
    CELL cat[LOOKUP_COLORS];
    struct _Color_Info_ *cp;

    if (mod)
	cp = &colors->modular;
    else
	cp = &colors->fixed;

    if (cp->lookup.active)
	return;

    n = cp->max - cp->min + 1;
    if (n >= LOOKUP_COLORS || n <= 0)
	return;

    x = cp->min;
    for (i=0; i < n; i++)
	cat[i] = x++;;
    
    cp->lookup.nalloc = n;
    cp->lookup.red = (unsigned char *) G_malloc(n);
    cp->lookup.grn = (unsigned char *) G_malloc(n);
    cp->lookup.blu = (unsigned char *) G_malloc(n);
    cp->lookup.set = (unsigned char *) G_malloc(n);

    G_zero (cp->lookup.set, n*sizeof(unsigned char));
    G__lookup_colors (cat,
	    cp->lookup.red, cp->lookup.grn, cp->lookup.blu, cp->lookup.set,
	    n, colors, mod, 1);

    *cat = 0;
    cp->lookup.s0 = 0;
    G__lookup_colors (cat,
	    &cp->lookup.r0, &cp->lookup.g0, &cp->lookup.b0, &cp->lookup.s0,
	    1, colors, mod, 1);
    cp->lookup.s0 = 1;

    cp->lookup.active = 1;
}
