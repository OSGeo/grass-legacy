#include "tree.h"

init_group (group)
    struct Group *group;
{
    group->min = 0 ;
    group->max = -1 ;
}

mark_group (group, lo, hi)
    struct Group *group;
{
    int curlen, newlen;
    int i, gap;

    if (lo > hi)
    {
	i = lo;
	lo = hi;
	hi = i;
    }

    if (group->min > group->max)
    {
	newlen = hi - lo + 1;
	group->table = G_malloc (newlen);
	group->min = lo;
	group->max = hi;
    }
    if (hi > group->max)
    {
	curlen = group->max - group->min + 1;
	newlen = hi - group->min + 1;

	group->table = G_realloc (group->table, newlen);

    /* fill in gap with zero */
	for (i = curlen; i < newlen; i++)
	    group->table[i] = 0;

	group->max = hi;
    }
    if (lo < group->min)
    {
	curlen = group->max - group->min + 1;
	newlen = group->max - lo + 1;
	gap    = newlen - curlen;

	group->table = G_realloc (group->table, newlen);

    /* shift the table to make room in front */
	for (i = 1; i <= curlen; i++)
	    group->table[newlen-i] = group->table[curlen-i];

    /* fill in gap with zero */
	for (i=1; i < gap; i++)
	    group->table[i] = 0 ;

	group->min = lo;
    }

/* set the values! */
    for (i = lo; i <= hi; i++)
	group->table[i-group->min] = 1;
}
