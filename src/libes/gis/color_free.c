#include "gis.h"

G_free_colors (colors)
    struct Colors *colors;
{
    G__color_reset (colors);
    G_init_colors (colors);
}

/*******************************************
 * G__color* routines only to be used by other routines in this
 * library
 *******************************************/

G__color_free_rules (cp)
    struct _Color_Info_ *cp;
{
    struct _Color_Rule_ *rule, *next;

    for (rule = cp->rules; rule; rule = next)
    {
	next = rule->next;
	free (rule);
    }
    cp->rules = NULL;
}

G__color_free_lookup (cp)
    struct _Color_Info_ *cp;
{
    if (cp->lookup.active)
    {
	free (cp->lookup.red);
	free (cp->lookup.blu);
	free (cp->lookup.grn);
	cp->lookup.active = 0;
    }
}

G__color_reset (colors)
    struct Colors *colors;
{
    G__color_free_lookup(&colors->fixed);
    G__color_free_lookup(&colors->modular);
    G__color_free_rules(&colors->fixed);
    G__color_free_rules(&colors->modular);
    colors->version = 0;
	/* this routine should NOT init the colors */
}
