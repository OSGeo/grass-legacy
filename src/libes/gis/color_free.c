#include <stdlib.h>
#include "gis.h"

int G_free_colors (
    struct Colors *colors)
{
    G__color_reset (colors);
    G_init_colors (colors);

    return 0;
}

/*******************************************
 * G__color* routines only to be used by other routines in this
 * library
 *******************************************/

int G__color_free_rules (
    struct _Color_Info_ *cp)
{
    struct _Color_Rule_ *rule, *next;

    for (rule = cp->rules; rule; rule = next)
    {
	next = rule->next;
	free (rule);
    }
    cp->rules = NULL;

    return 0;
}

int G__color_free_lookup (
    struct _Color_Info_ *cp)
{
    if (cp->lookup.active)
    {
	free (cp->lookup.red);
	free (cp->lookup.blu);
	free (cp->lookup.grn);
	free (cp->lookup.set);
	cp->lookup.active = 0;
    }

    return 0;
}

int G__color_free_fp_lookup ( struct _Color_Info_ *cp)
{
    if (cp->fp_lookup.active)
    {
	free (cp->fp_lookup.vals);
	free (cp->fp_lookup.rules);
	cp->fp_lookup.active = 0;
	cp->fp_lookup.nalloc = 0;
    }

    return 0;
}

int G__color_reset (
    struct Colors *colors)
{
    G__color_free_lookup(&colors->fixed);
    G__color_free_lookup(&colors->modular);
    G__color_free_rules(&colors->fixed);
    G__color_free_rules(&colors->modular);
    colors->version = 0;
	/* this routine should NOT init the colors */

    return 0;
}
