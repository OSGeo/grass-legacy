#include <string.h>
#include "rule.h"

extern CELL DEFAULT;
extern int default_rule, default_to_itself;
extern char *default_label;

int reclass (char *old_name, char *old_mapset,
    char *new_name, RULE *rules, struct Categories *cats, char *title)
{
    struct Reclass old, new;
    struct History hist;
    int is_reclass;
    FILE *fd;
    char buf[256];

    is_reclass = G_get_reclass (old_name, old_mapset, &old);
    if (is_reclass < 0)
    {
	sprintf (buf, "%s in %s - can't read header file", old_name, old_mapset);
	G_fatal_error (buf);
	exit(1);
    }

    if (is_reclass)
    {
	strcpy (new.name, old.name);
	strcpy (new.mapset, old.mapset);
	re_reclass (rules, cats, &old, &new, old_name, old_mapset);
    }
    else
    {
	strcpy (new.name, old_name);
	strcpy (new.mapset, old_mapset);
	_reclass (rules, cats, &old, &new);
    }

    if (G_put_reclass (new_name, &new) < 0)
    {
	sprintf (buf, "%s - unable to create reclass file", new_name);
	G_fatal_error (buf);
	exit(1);
    }

    if (title == NULL)
	sprintf (title = buf, "Reclass of %s in %s", new.name, new.mapset);

    if ((fd = G_fopen_new ("cell", new_name)) == NULL)
    {
	sprintf (buf, "%s - unable to create cell file", new_name);
	G_fatal_error (buf);
	exit(1);
    }
    fprintf (fd, "Don't remove me\n");
    fclose (fd);

    G_set_cats_title (title, cats);
    if (G_write_cats (new_name, cats) == -1)
    {
	sprintf (buf, "%s - unable to create category file", new_name);
	G_fatal_error (buf);
	exit(1);
    }
    G_free_cats (cats);

    G_short_history (new_name, "reclass", &hist);
    strcpy (hist.datsrc_1, "Reclassified map based on:");
    sprintf (hist.datsrc_2, "  Map [%s] in mapset [%s]",
	new.name, new.mapset);
    G_write_history (new_name, &hist);

    new_range (new_name, &new);

    return 0;
}

int _reclass (RULE *rules, struct Categories *cats,
    struct Reclass *old, struct Reclass *new)
{
    RULE *r;
    register CELL i;
    register CELL n;
    int first, cats_read=0;
    int *is_default;
    /* wether or not the rule for this category is explicitly specified */
    long num;
    struct Range range;
    struct Categories old_cats;
/*
 * Do not uncomment this code unless you have a good reason!
 *
 * Supposing this case:
 * 
 * map range is 0 to 4 and given rules:
 *   r new
 *   1=0
 *   3=1
 *   4=2
 *   7=4		# how can we check every range of input maps
 *   *=0
 *
 * In this case, new->min = 0, new->max = 4, new->num = 4 - 0 + 1 = 5;
 * new->table size = is_default size = 5 of sizeof(DATA TYPE)
 *
 * OK, another fragment of code tries to assign table:
 *
 * for ( r = rules; r; r = r->next)
 *    for ( i = r->lo; i <= r->hi; i++)
 *    {
 *	  n = i;
 *
 *	  new->table[n-new->min] = r->new;
 *	  is_default[n-new->min] = 0;
 *
 * its index may be 1-1=0
 * 		    3-1=2
 * 		    4-1=3
 * 		    7-1=6	limit exceeded!!! index range: 0 to 4
 *
 * JUST DO NOT UNCOMMENT THIS CODE !!
 *
 *
    if(default_rule && !G_is_c_null_value(&DEFAULT))
    {
         first = 0;
         G_read_range(new->name, new->mapset, &range);
         G_get_range_min_max(&range, &new->min, &new->max);
         if(G_is_c_null_value(&new->min) || 
            G_is_c_null_value(&new->max))
            G_fatal_error("input range is empty!");
         if(default_to_itself)
         {
            if(G_read_cats (new->name, new->mapset, &old_cats)< 0)
               cats_read = 0;
            else cats_read = 1;;
         }
     }
     else
*/
     {
         /* first find the min,max cats */
         first = 1;
         for (r = rules; r; r = r->next)
         {
	    for (i = r->lo; i <= r->hi; i++)
	    {
	        n = i;
            /* assign min,max */
	        if (first)
	        {
	    	    new->min = new->max = n;
		    first = 0;
	        }
	        else
	        {
	    	    if (n < new->min) new->min = n;
		    if (n > new->max) new->max = n;
	        }
	    }
        }
    } /* no default */

 /* make sure we have at least one entry */
    if (first) new->min = new->max = 0;

 /* allocate reclass table */
    new->type = RECLASS_TABLE;
    num = new->max - new->min + 1;
    if (num != (int) num)		/* make sure don't overflow int */
	G_fatal_error ("Too many categories");

    new->num = num;
    new->table = (CELL *) G_calloc (new->num, sizeof(CELL));
    is_default = (int *) G_calloc (new->num, sizeof(int));
    for (i = 0; i < new->num; i++)
    {
       if(default_rule)
       {
           if(default_to_itself)
                  new->table[i] = i + new->min;
             else
                  new->table[i] = DEFAULT;
           is_default[i] = 1;
           if(G_is_c_null_value(&new->table[i])) continue;
           if (cats->num < new->table[i])
                  cats->num = new->table[i];
       }
       else
       {
           G_set_c_null_value(&new->table[i], 1);
           is_default[i] = 0;
       }
   }

/* generate the new reclass lookup table */
     for (r = rules; r; r = r->next)
	for (i = r->lo; i <= r->hi; i++)
	{
	    n = i;
	    new->table[n-new->min] = r->new;
	    is_default[n-new->min] = 0;
	    if(G_is_c_null_value(&r->new)) continue;
	    if (cats->num < r->new)
		cats->num = r->new;
	}
 
     /* now set the default categories */
     /* the new cats set in rules should already be set in parse.c */
     if(default_to_itself && cats_read)
     {
         for (i = 0; i < new->num; i++)
         {
             if(is_default[i])
             {
               G_set_cat(i + new->min, 
                  G_get_cat (i + new->min, &old_cats), cats);
             }
         }
     }
     else if(default_rule)
        G_set_cat(DEFAULT, default_label, cats);

     return 0;
}

int 
re_reclass (RULE *rules, struct Categories *cats, struct Reclass *old, struct Reclass *new, char *input_name, char *input_mapset)
{
    RULE *r;
    register CELL i;
    register CELL n;
    long num;
    int *is_default;
     /* wether or not the rule for this category is explicitly specified */
    int cats_read=0;
    CELL old_min_cat, old_max_cat;
    struct Categories old_cats;

    /* we will build a new table first from the old table, then
     * modify it as neccessary to shrink it 
     */

    if(default_to_itself)
    {
       if(G_read_cats (input_name, input_mapset, &old_cats)< 0)
               cats_read = 0;
       else cats_read = 1;
    }

    /* first make a copy of old table */
    new->table = (CELL *) G_calloc (old->num, sizeof(CELL));

    cats->num = 0;
    cats->num = 0;
    old_min_cat = old_max_cat = old->table[old->min];;
    for (i = 0; i < old->num; i++)
    {
	if(G_is_c_null_value(&(old->table[i - old->min]))) continue;
        if(old_min_cat > old->table[i - old->min]) 
                old_min_cat = old->table[i - old->min];
        if(old_max_cat < old->table[i - old->min]) 
                old_max_cat = old->table[i - old->min];
    }
    /* list of flags for all old category values */
    if(old_max_cat -old_min_cat +1 > old->num)
          is_default = (int *) G_calloc (old_max_cat -old_min_cat +1, sizeof(int));
    else
	  is_default = (int *) G_calloc (old->num, sizeof(int));
    for (i = old_min_cat; i <= old_max_cat; i++)
       is_default[i - old_min_cat] = 1;

    /* initialize the new table */
    for (i = 0; i < old->num; i++)
    {
      if(default_rule)
      {
          if(default_to_itself)
                 new->table[i] = old->table[i];
          else
                 new->table[i] = DEFAULT;
          if(G_is_c_null_value(&new->table[i])) continue;
          if (cats->num < new->table[i])
              cats->num = new->table[i];
      }
      else
      {
          G_set_c_null_value(&new->table[i], 1);
          is_default[i] = 0;
      }
    }

/* generate the new reclass lookup table */
    for (r = rules; r; r = r->next)
	for (i = r->lo; i <= r->hi; i++)
	{
	    for (n = old->min ; n <= old->max ; n++)
	    {
		if (old->table[n-old->min] == i)
		{
		    new->table[n-old->min] = r->new;
                    is_default[i-old_min_cat] = 0;
                    if(G_is_c_null_value(&r->new)) continue;
		    if (cats->num < r->new)
			cats->num = r->new;
		}
	    }
	}

    /* now go thru and clean up new reclass table */

    for (i = old->min ; G_is_c_null_value(&new->table[i - old->min]) && i <= old->max ; i++);

    if (i > old->max)	/* is entire table zero ? */
	new->min = new->max = 0;
    else
    {
	new->min = i;
        for (i = old->max ; G_is_c_null_value(&new->table[i - old->min]) && i >= old->min ; i--);
	    ;
	new->max = i;
    }
    if (new->min || new->max)
	new->table = &(new->table[new->min - old->min]);

 /* allocate reclass table */
    new->type = RECLASS_TABLE;
    num = new->max - new->min + 1;
    if (num != (int) num)		/* make sure don't overflow int */
	G_fatal_error ("Too many categories");
    new->num = num;

    /* now set the default categories */
    /* the new cats set in rules should already be set in parse.c */
    if(default_to_itself && cats_read)
    {
        for (i = old_min_cat; i <= old_max_cat; i++)
        {
           if(is_default[i])
               G_set_cat(i , G_get_cat (i, &old_cats), cats);
        }
    }
    else if(default_rule)
      G_set_cat(DEFAULT, default_label, cats);

    return 0;
}
