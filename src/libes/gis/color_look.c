#include <math.h>
#include "gis.h"

/* old 4.1 routine */
int G_lookup_colors (
    CELL *cell,
    unsigned char *red,unsigned char *grn,unsigned char *blu,
    unsigned char *set,
    int n,
    struct Colors *colors)
{
    G_lookup_c_raster_colors (cell, red, grn, blu, set, n, colors);

    return 0;
}

/* I don't think it should exist, because it requires openning
   of raster file every time Olga 
int G_lookup_rgb_colors(map, mapset, r, g, b)
    char *name, *mapset;
    unsigned char *r, *g, *b;
{
    RASTER_MAP_TYPE map_type;
    void *rast;
    ....
}
*/

int G_lookup_c_raster_colors (
    CELL *cell,
    unsigned char *red, unsigned char *grn, unsigned char *blu,
    unsigned char *set,
    int n,
    struct Colors *colors)
{
    G__organize_colors (colors); /* make sure the lookup tables are in place */

    G_zero ((char *) set, n*sizeof(unsigned char));

/* first lookup the fixed colors */
    G__lookup_colors ((void *)cell, red, grn, blu, set, n, colors, 0, 0, CELL_TYPE);

/* now lookup unset colors using the modular rules */
    G__lookup_colors ((void *)cell, red, grn, blu, set, n, colors, 1, 0, CELL_TYPE);

    return 0;
}

int G_lookup_raster_colors (
    void *raster,
    unsigned char *red, unsigned char *grn, unsigned char *blu,
    unsigned char *set,
    int n,
    struct Colors *colors,
    RASTER_MAP_TYPE map_type)
{
    G__organize_colors (colors); /* make sure the lookup tables are in place */
    /* in case of float color rules, fp_lookup table is created */

    G_zero ((char *) set, n*sizeof(unsigned char));

/* first lookup the fixed colors */
    G__lookup_colors (raster, red, grn, blu, set, n, colors, 0, 0, map_type);

/* now lookup unset colors using the modular rules */
    G__lookup_colors (raster, red, grn, blu, set, n, colors, 1, 0, map_type);

    return 0;
}

int G_lookup_f_raster_colors (fcell, red, grn, blu, set, n, colors)
    FCELL *fcell;
    unsigned char *red, *grn, *blu, *set;
    int n;
    struct Colors *colors;
{
    G__organize_colors (colors); /* make sure the lookup tables are in place */
    /* in case of float color rules, fp_lookup table is created */

    G_zero ((char *) set, n*sizeof(unsigned char));

/* first lookup the fixed colors */
    G__lookup_colors ((void *) fcell, red, grn, blu, set, n, colors, 0, 0, FCELL_TYPE);

/* now lookup unset colors using the modular rules */
    G__lookup_colors ((void *) fcell, red, grn, blu, set, n, colors, 1, 0, FCELL_TYPE);

    return 0;
}

int G_lookup_d_raster_colors (dcell, red, grn, blu, set, n, colors)
    DCELL *dcell;
    unsigned char *red, *grn, *blu, *set;
    int n;
    struct Colors *colors;
{
    G__organize_colors (colors); /* make sure the lookup tables are in place */
    /* in case of float color rules, fp_lookup table is created */

    G_zero ((char *) set, n*sizeof(unsigned char));

/* first lookup the fixed colors */
    G__lookup_colors ((void *) dcell, red, grn, blu, set, n, colors, 0, 0, DCELL_TYPE);

/* now lookup unset colors using the modular rules */
    G__lookup_colors ((void *) dcell, red, grn, blu, set, n, colors, 1, 0, DCELL_TYPE);

    return 0;
}


static int less_or_equal(x, y)
   double x, y;
{
   if(x<=y) return 1;
   else return 0;
}
		
static int less(x, y)
   double x, y;
{
   if(x<y) return 1;
   else return 0;
}
		

int G__lookup_colors (raster, red, grn, blu, set, n, colors, mod, rules_only, data_type)
    void *raster;
    unsigned char *red, *grn, *blu, *set;
    int n;
    RASTER_MAP_TYPE data_type;
    struct Colors *colors;
{
    struct _Color_Info_ *cp;
    struct _Color_Rule_ *rule;
    DCELL dmin, dmax, val, dmod=0L, shift;
    CELL cat, min, max;
    register void *ptr, *last_ptr=NULL;
    int invert;
    int found, r, g, b;
    int cell_type;
    int lookup, max_ind, min_ind, try;
    int (*lower)();
    
    if (mod)
	cp = &colors->modular;
    else
	cp = &colors->fixed;

/* rules_only will be true only when called by G__organize_colors()
 * when building the integer lookup talbes from the rules,
 * so do not shift, invert, use lookup table or modulate cats.
 * these operations will happen when lookup is called by user code
 */
    /* we want min, max for cp, not min, max overall */
    dmin = cp->min;
    dmax = cp->max;
    min = floor(dmin);
    max = ceil(dmax);

    cell_type = (data_type == CELL_TYPE);

    if (rules_only)
    {
	shift = invert = lookup = mod = 0;
    }
    else
    {
	if(mod)
        {
	   dmod = dmax - dmin;
        /* for integers color table we make a gap of 1 in order
           to make the same colors as before */
           if(cell_type)
               dmod += 1;
        }

	shift  = colors->shift;
	invert = colors->invert;
	lookup = cp->lookup.active;
    }

    ptr = raster;

    for (; n-- > 0; ptr = G_incr_void_ptr(ptr, G_raster_size(data_type)), red++, grn++, blu++, *set++ = found)
    {
        /* if the cell is the same as last one, use the prev color values */  
	if(ptr != raster && G_raster_cmp(ptr, last_ptr, data_type) == 0)
	{
	   *red = *(red-1);
	   *blu = *(blu-1);
	   *grn = *(grn-1);
	   found = *(set-1);
	   last_ptr = ptr;
	   continue;
        }
	val = G_get_raster_value_d(ptr, data_type);
     /* DEBUG fprintf (stderr, "val: %.4lf\n", val); */
	last_ptr = ptr;

	if (*set)
	{
	    found = 1;
  	    continue;
        }

	if (G_is_null_value(ptr, data_type))
	{
	    /* returns integers, not unsigned chars */
	    G_get_null_value_color(&r, &g, &b, colors);
	    *red = r; *grn = g; *blu = b;
	    found = 1;
	    continue;
	}

	if (shift && val >= dmin && val <= dmax)
	{
	    val += shift;
	    while (val < dmin)
		val += dmax - dmin + 1;
	    while (val > dmax)
		val -= dmax - dmin + 1;
	}

/* invert non-null data around midpoint of range [min:max] */
	if (invert)
	    val = dmin + dmax - val;

        if(mod)
	{
	    if(dmod > 0)
	    {
	        val -= dmin;
	        while (val < 0)
		     val += dmod;
                val = val - dmod * floor(val/dmod);
	        val += dmin;
	    }
	    else
		val = dmin;
	}

	cat = (CELL) val;

	found = 0;

        /* for non-null integers  try to look them up in lookup table */
	/* note: lookup table exists only for integer maps, and we also must
	check if val is really integer */

	if (lookup && ((double) cat - val == 0.))
	{
	    if (cat >= min && cat <= max)
	    {
		cat -= min;
		if (cp->lookup.set[cat])
		{
		    *red = cp->lookup.red[cat];
		    *grn = cp->lookup.grn[cat];
		    *blu = cp->lookup.blu[cat];
		    found = 1;
/*DEBUG
	fprintf (stderr, "lookup %d %.2lf %d %d %d\n\n", cat, val, *red, *grn, *blu);
	*/
		}
	    }
	}

	if (found) 
	   continue;

	/* if floating point lookup table is active, look up in there */
	if(cp->fp_lookup.active)
	{
	   try = (cp->fp_lookup.nalloc-1)/2;
	   min_ind = 0;
	   max_ind = cp->fp_lookup.nalloc-2;
	   while(1)
	   {
	   /* when the rule for the interval is NULL, we exclude the end points.
              when it exists, we include the end-points */
	      if(cp->fp_lookup.rules[try])
		    lower = less;
              else
		    lower = less_or_equal;
              /* DEBUG
	      fprintf (stderr, "%d %d %d %lf %lf %lf\n", min_ind, try, max_ind,
					       cp->fp_lookup.vals[try-1],
					       val,
					       cp->fp_lookup.vals[try]);
              */
			
	      if (lower(cp->fp_lookup.vals[try+1], val))
	      { /* recurse to the second half */
		 min_ind = try+1;
	        /* must be still < nalloc-1, since number is within the range */
		 try = (max_ind + min_ind)/2;
		 if(min_ind > max_ind)
		 {
		    rule = NULL;
		    break;
                 }
		 continue;
              }
	      if (lower(val, cp->fp_lookup.vals[try]))
	      { /* recurse to the second half */
		 max_ind = try-1;
		 /* must be still >= 0, since number is within the range */
		 try = (max_ind + min_ind)/2;
		 if(max_ind < min_ind)
		 {
		    rule = NULL;
		    break;
                 }
		 continue;
              }
	      rule = cp->fp_lookup.rules[try];
	      break;
	   }
        }
        else     
	{
            /* find the [low:high] rule that applies */
	    for (rule = cp->rules; rule; rule = rule->next)
	    {
	    /* DEBUG
	        fprintf (stderr, "%.2lf %.2lf %.2lf\n", 
		val, rule->low.value, rule->high.value);
	    */
	       if (rule->low.value <= val && val <= rule->high.value)
	   	   break;
            }
	}

/* if found, perform linear interpolation from low to high.
 * else set colors to colors->undef or white if undef not set
 */

	if (rule)
	{
	    G__interpolate_color_rule (val, red, grn, blu, rule);
	    found = 1;
        }
	if (!found)
	{
            /* otherwise use default color */
	    G_get_default_color(&r, &g, &b, colors);
	    *red = r; *grn = g; *blu = b;
        }
/* DEBUG
	if (rule)
        fprintf (stderr, "%.2lf %d %d %d   %.2lf %d %d %d \n", rule->low.value , (int)rule->low.red, (int)rule->low.grn, (int)rule->low.blu, rule->high.value, (int)rule->high.red, (int)rule->high.grn, (int)rule->high.blu);
        fprintf (stderr, "rule found %d %.2lf %d %d %d\n\n", cat, val, *red, *grn, *blu);
*/
    }

    return 0;
}

int G__interpolate_color_rule (val, red, grn, blu, rule)
    DCELL val;
    unsigned char *red, *grn, *blu;
    struct _Color_Rule_ *rule;
{
    DCELL delta;

    if((delta = rule->high.value - rule->low.value))
    {
	val -= rule->low.value;

	*red = (int) (val * (double) ((int)rule->high.red - (int)rule->low.red)/ delta)
		    + (int)rule->low.red;
	*grn = (int) (val * (double) ((int)rule->high.grn - (int)rule->low.grn)/ delta )
		    + (int)rule->low.grn;
	*blu = (int) (val * (double) ((int)rule->high.blu - (int)rule->low.blu)/ delta )
		    + (int)rule->low.blu;
    }
    else
    {
	*red = rule->low.red;
	*grn = rule->low.grn;
	*blu = rule->low.blu;
    }

    return 0;
}

