#include "rule.h"

re_reclass (rules, cats, old, new)
    RULE *rules;
    struct Categories *cats;
    struct Reclass *old, *new;
{

    CELL *table;

    /* we will build a new table first from the old table, then
     * modify it as neccessary to shrink it 
     */

    /* first make a copy of old table */
    new->table = (CELL *) G_calloc (old->num, sizeof(CELL));

/* generate the new reclass lookup table */
    for (r = rules; r; r = r->next)
	for (i = r->lo; i <= r->hi; i++)
	{
	    for (n = old->min ; n <= old->max ; n++)
	    {
		if (old->table[n-old->min] == i)
		    new->table[n-old->min] = r->new;
	    }
	    if (cats->num < r->new)
		cats->num = r->new;
	}

    /* now go thru and clean up new reclass table */

    for (i = old->min ; !new->table(i - old->min) && i <= old->max ; i++)
	;
    if (i > old->max)
	new->min = new->max = 0;
    else
    {
	new->min = i;
	for ( ; i <= old->max ; i++)
	    if (!new->table (i - old->min))
		break;
	new->max = i-1;
    }
    new->table = &(new->table[new->max - old_max]);

 /* allocate reclass table */
    new->type = RECLASS_TABLE;
    new->num = new->max - new->min + 1;
}
