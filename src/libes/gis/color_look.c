#include "gis.h"

static int bigint = 0;

G_lookup_colors (cell, red, grn, blu, set, n, colors)
    CELL *cell;
    unsigned char *red, *grn, *blu, *set;
    int n;
    struct Colors *colors;
{
    G__organize_colors (colors); /* make sure the lookup tables are in place */

    G_zero (set, n*sizeof(unsigned char));

/* first lookup the fixed colors */
    G__lookup_colors (cell, red, grn, blu, set, n, colors, 0, 0);

/* now lookup unset colors using the modular rules */
    G__lookup_colors (cell, red, grn, blu, set, n, colors, 1, 0);
}

G__lookup_colors (cell, red, grn, blu, set, n, colors, mod, rules_only)
    CELL *cell;
    unsigned char *red, *grn, *blu, *set;
    int n;
    struct Colors *colors;
{
    struct _Color_Info_ *cp;
    struct _Color_Rule_ *rule;
    CELL cat, delta;
    CELL min, max;
    int shift, invert;
    int found;
    int lookup;


    if (mod)
	cp = &colors->modular;
    else
	cp = &colors->fixed;

/* rules_only will be true only when called by G__organize_colors()
 * when building the lookup talbes from the rules,
 * so do not shift, invert, use lookup table or modulate cats.
 * these operations will happen when lookup is called by user code
 */
    if (rules_only)
    {
	shift = invert = lookup = mod = 0;
    }
    else
    {
	if(mod)
	{
	    mod = cp->max - cp->min + 1;
	    if (mod <= 0) return;
	}
	shift  = colors->shift;
	invert = colors->invert;
	lookup = cp->lookup.active;
    }
    min = colors->cmin;
    max = colors->cmax;

    if (bigint==0)compute_bigint();

    for (; n-- > 0; red++, grn++, blu++, *set++ = found)
    {
	cat = *cell++;
	if (*set)
	    continue;

/* shift non-zero data, wrap into range [min:max] */
	if (shift && cat && cat >= min && cat <= max)
	{
	    cat += shift;
	    while (cat < min)
		cat += max - min + 1;
	    while (cat > max)
		cat -= max - min + 1;
	}

/* invert non-zero data around midpoint of range [min:max] */
	if (invert && cat)
	    cat = min + max - cat;

	if (mod && cat)
	{
	    cat -= cp->min;
	    while (cat < 0)
		cat += mod;
	    cat %= mod;
	    cat += cp->min;
	}
	found = 0;
	if (lookup)
	{
	    if (cat == 0)
	    {
		if(cp->lookup.s0)
		{
		    *red = cp->lookup.r0;
		    *grn = cp->lookup.g0;
		    *blu = cp->lookup.b0;
		    found = 1;
		}
	    }
	    else if (cat >= cp->min && cat <= cp->max)
	    {
		cat -= cp->min;
		if (cp->lookup.set[cat])
		{
		    *red = cp->lookup.red[cat];
		    *grn = cp->lookup.grn[cat];
		    *blu = cp->lookup.blu[cat];
		    found = 1;
		}
	    }
	}
	if (found) continue;

/* find the [low:high] rule that applies */
	for (rule = cp->rules; rule; rule = rule->next)
	{
	    if (rule->low.cat <= cat && cat <= rule->high.cat)
		break;
	}

/* if found, perform linear interpolation from low to high.
 * else set colors to white
 */

	if (rule)
	{
	    if(delta = rule->high.cat - rule->low.cat)
	    {
		cat -= rule->low.cat;

		/* big numbers will overflow. divide by 256 first */
		if (delta >= bigint || delta <= -bigint)
		{
		    delta /= 256;
		    cat   /= 256;
		}

		*red = cat * ((int)rule->high.red - (int)rule->low.red) / delta
			    + (int)rule->low.red;
		*grn = cat * ((int)rule->high.grn - (int)rule->low.grn) / delta 
			    + (int)rule->low.grn;
		*blu = cat * ((int)rule->high.blu - (int)rule->low.blu) / delta 
			    + (int)rule->low.blu;
	    }
	    else
	    {
		*red = rule->low.red;
		*grn = rule->low.grn;
		*blu = rule->low.blu;
	    }
	    found = 1;
	}
	if (!found)
	    *red = *blu = *grn = 255; /* white */
    }
}

G__interpolate_color_rule (cat, red, grn, blu, rule)
    CELL cat;
    unsigned char *red, *grn, *blu;
    struct _Color_Rule_ *rule;
{
    CELL delta;

    if (bigint==0)compute_bigint();

    if(delta = rule->high.cat - rule->low.cat)
    {
	cat -= rule->low.cat;

	/* big numbers will overflow. divide by 256 first */
	if (delta >= bigint || delta <= -bigint)
	{
	    delta /= 256;
	    cat   /= 256;
	}

	*red = cat * ((int)rule->high.red - (int)rule->low.red) / delta
		    + (int)rule->low.red;
	*grn = cat * ((int)rule->high.grn - (int)rule->low.grn) / delta 
		    + (int)rule->low.grn;
	*blu = cat * ((int)rule->high.blu - (int)rule->low.blu) / delta 
		    + (int)rule->low.blu;
    }
    else
    {
	*red = rule->low.red;
	*grn = rule->low.grn;
	*blu = rule->low.blu;
    }
}

static
compute_bigint()
{
    int i;
    bigint = 128;
    for (i = 2; i < sizeof(int); i++)
	bigint *= 256;
}
