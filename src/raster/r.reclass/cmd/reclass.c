#include "rule.h"

reclass (old_name, old_mapset, new_name, rules, cats, title)
    char *old_name, *old_mapset, *new_name;
    RULE *rules;
    struct Categories *cats;
    char *title;
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
	re_reclass (rules, cats, &old, &new);
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
    G_set_cats_title (title, cats);
    if (G_write_cats (new_name, cats) == -1)
    {
	sprintf (buf, "%s - unable to create category file", new_name);
	G_fatal_error (buf);
	exit(1);
    }
    G_free_cats (cats);

    if ((fd = G_fopen_new ("cell", new_name)) == NULL)
    {
	sprintf (buf, "%s - unable to create cell file", new_name);
	G_fatal_error (buf);
	exit(1);
    }

    fprintf (fd, "Don't remove me\n");
    fclose (fd);

    G_short_history (new_name, "reclass", &hist);
    strcpy (hist.datsrc_1, "Reclassified map based on:");
    sprintf (hist.datsrc_2, "  Map [%s] in mapset [%s]",
	new.name, new.mapset);
    G_write_history (new_name, &hist);

    new_range (new_name, &new);
}

_reclass (rules, cats, old, new)
    RULE *rules;
    struct Categories *cats;
    struct Reclass *old, *new;
{
    RULE *r;
    register CELL i;
    register CELL n;
    int first;
    long num;

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

 /* make sure we have at least one entry */
    if (first) new->min = new->max = 0;

 /* allocate reclass table */
    new->type = RECLASS_TABLE;
    num = new->max - new->min + 1;
    if (num != (int) num)		/* make sure don't overflow int */
	G_fatal_error ("Too many categories");

    new->num = num;
    new->table = (CELL *) G_calloc (new->num, sizeof(CELL));
    for (i = 0; i < new->num; i++)
	new->table[i] = 0;

/* generate the new reclass lookup table */
    for (r = rules; r; r = r->next)
	for (i = r->lo; i <= r->hi; i++)
	{
	    n = i;
	    new->table[n-new->min] = r->new;
	    if (cats->num < r->new)
		cats->num = r->new;
	}
}

re_reclass (rules, cats, old, new)
    RULE *rules;
    struct Categories *cats;
    struct Reclass *old, *new;
{
    RULE *r;
    register CELL i;
    register CELL n;
    long num;

    /* we will build a new table first from the old table, then
     * modify it as neccessary to shrink it 
     */

    /* first make a copy of old table */
    new->table = (CELL *) G_calloc (old->num, sizeof(CELL));

    cats->num = 0;
/* generate the new reclass lookup table */
    for (r = rules; r; r = r->next)
	for (i = r->lo; i <= r->hi; i++)
	{
	    for (n = old->min ; n <= old->max ; n++)
	    {
		if (old->table[n-old->min] == i)
		{
		    new->table[n-old->min] = r->new;
		    if (cats->num < r->new)
			cats->num = r->new;
		}
	    }
	}

    /* now go thru and clean up new reclass table */

    for (i = old->min ; !new->table[i - old->min] && i <= old->max ; i++)
	;
    if (i > old->max)	/* is entire table zero ? */
	new->min = new->max = 0;
    else
    {
	new->min = i;
	for (i = old->max ; !new->table[i - old->min] && i >= old->min ; i--)
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
}
