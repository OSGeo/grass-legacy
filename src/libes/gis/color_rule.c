#include "gis.h"

#define LIMIT(x) if (x < 0) x = 0; else if (x > 255) x = 255;

G_add_color_rule (cat1, r1,g1,b1, cat2, r2,g2,b2, colors)
    CELL cat1,cat2;
    int r1,g1,b1;
    int r2,g2,b2;
    struct Colors *colors;
{
    add_color_rule (cat1, r1,g1,b1, cat2, r2,g2,b2, &colors->fixed, colors->version,
	&colors->cmin, &colors->cmax);
    return 1;
}

G_add_modular_color_rule (cat1, r1,g1,b1, cat2, r2,g2,b2, colors)
    CELL cat1,cat2;
    int r1,g1,b1;
    int r2,g2,b2;
    struct Colors *colors;
{
    CELL min, max;
    if (colors->version < 0)
	return -1; /* can;t use this on 3.0 colors */
    min = colors->cmin;
    max = colors->cmax;
    add_color_rule (cat1, r1,g1,b1, cat2, r2,g2,b2, &colors->modular, 0,
	&colors->cmin, &colors->cmax);
    colors->cmin = min; /* don't reset these */
    colors->cmax = max;
	
    return 1;
}

static
add_color_rule (cat1, r1,g1,b1, cat2, r2,g2,b2, cp, version, cmin, cmax)
    CELL cat1,cat2;
    int r1,g1,b1;
    int r2,g2,b2;
    struct _Color_Info_ *cp;
    CELL *cmin, *cmax;
{
    struct _Color_Rule_ *rule, *next;
    unsigned char red, grn, blu;
    CELL min,max,cat;

/* allocate a low:high rule */
    rule = (struct _Color_Rule_ *) G_malloc (sizeof(*rule));
    rule->next = rule->prev = NULL;

/* make sure colors are in the range [0,255] */
    LIMIT(r1);
    LIMIT(g1);
    LIMIT(b1);
    LIMIT(r2);
    LIMIT(g2);
    LIMIT(b2);

/* cat1==cat2, use average color */
/* otherwise make sure low < high */
    if (cat1 == cat2)
    {
	rule->low.cat = rule->high.cat = cat1;
	rule->low.red = rule->high.red = (r1+r2)/2;
	rule->low.grn = rule->high.grn = (g1+g2)/2;
	rule->low.blu = rule->high.blu = (b1+b2)/2;
    }
    else if (cat1 < cat2)
    {
	rule->low.cat = cat1;
	rule->low.red = r1;
	rule->low.grn = g1;
	rule->low.blu = b1;

	rule->high.cat = cat2;
	rule->high.red = r2;
	rule->high.grn = g2;
	rule->high.blu = b2;
    }
    else
    {
	rule->low.cat = cat2;
	rule->low.red = r2;
	rule->low.grn = g2;
	rule->low.blu = b2;

	rule->high.cat = cat1;
	rule->high.red = r1;
	rule->high.grn = g1;
	rule->high.blu = b1;
    }

/* keep track of the overall min and max, excluding zero */
    if ((min = rule->low.cat) == 0)
	min = 1;
    if ((max = rule->high.cat) == 0)
	max = -1;
    if (min <= max)
    {
	if (cp->min > cp->max)
	{
	    cp->min = min;
	    cp->max = max;
	}
	else
	{
	    if(cp->min > min)
		cp->min = min;
	    if(cp->max < max)
		cp->max = max;
	}
    }
    if (*cmin > *cmax)
    {
	*cmin = cp->min;
	*cmax = cp->max;
    }
    else
    {
	if(*cmin > cp->min)
	    *cmin = cp->min;
	if(*cmax < cp->max)
	    *cmax = cp->max;
    }

/* If version is old style (i.e., pre 4.0),
 *     interpolate this rule from min to max
 *     and insert each cat into the lookup table.
 *     Then free the rule.
 * Otherwise, free the lookup table, if active.
 *     G_organize_colors() will regenerate it
 *     Link this rule into the list of rules
 */

    if (version < 0)
    {
	if (rule->low.cat == 0 || rule->high.cat == 0)
	{
	    cat = 0;
	    G__interpolate_color_rule (cat, &red, &grn, &blu, rule);
	    G__insert_color_into_lookup (cat, (int)red, (int)grn, (int)blu, cp);
	}
	for (cat = min; cat <= max; cat++)
	{
	    G__interpolate_color_rule (cat, &red, &grn, &blu, rule);
	    G__insert_color_into_lookup (cat, (int)red, (int)grn, (int)blu, cp);
	}
	free (rule);
    }
    else
    {
	if (cp->rules)
	    cp->rules->prev = rule;
	rule->next = cp->rules;
	cp->rules = rule;

    /* prune the rules:
     * remove all rules that are contained by this rule 
     */
	min = rule->low.cat;  /* mod 4.1 */
	max = rule->high.cat; /* mod 4.1 */
	for (rule = rule->next; rule; rule = next)
	{
	    next = rule->next; /* has to be done here, not in for stmt */
	    if (min <= rule->low.cat && max >= rule->high.cat)
	    {
		if (rule->prev->next = next) /* remove from the list */
		    next->prev = rule->prev;
		free(rule);
	    }
	}
    
    /* free lookup array, if allocated */
	G__color_free_lookup(cp);
    }
}
