#include "global.h"

plant_tree()
{
    register CELL *cat;
    register STATS *stats;
    register int i;

    NODE *node;
    tree = (NODE *) G_calloc (tlen = INCR, sizeof (NODE));
    node = &tree[N = 1];

    cat   = node -> cat   = (CELL *) G_calloc (nfiles, sizeof(CELL));
    stats = node -> stats = (STATS *) G_calloc (NCATS, sizeof (STATS));

    i = nfiles;
    while(i--)
	*cat++ = 0;
    i = NCATS;
    while (i--)
    {
	stats->count = 0;
	stats->area  = 0.0;
	stats++;
    }
    node->left  = 0;
    node->right = 0;
}

first_node (cat, stats)
    CELL **cat;
    STATS **stats;
{
    register int p,q;

/* start at root and go all the way to the left */
    p = 1;
    while (q = tree[p].left)
	p = q;
    *cat = tree[p].cat;
    *stats = tree[p].stats;
    return p;
}

next_node (p, cat, stats)
    CELL **cat;
    STATS **stats;
{
    register int q;

/* go to the right */
    p = tree[p].right;

    if (p == 0)          /* no more */
	return 0;

    if (p < 0)           /* thread. stop here */
    {
	p = -p ;
	*cat = tree[p].cat;
	*stats = tree[p].stats;
	return p;
    }

    while (q = tree[p].left)   /* now go all the way left */
	p = q;

    *cat = tree[p].cat;
    *stats = tree[p].stats;
    return p;
}

CELL
index_cat (cat)
    register CELL cat;
{
    register CELL idx;

/*
    if (cat < 0)
	idx = -(-cat/NCATS) - 1;
    else
	idx = cat/NCATS;

    if ((idx *= NCATS) < 0) idx++;
*/
    if (cat < 0)
	idx = -((-cat) >> SHIFT) - 1;
    else
	idx = cat >> SHIFT;

    if (idx < 0)
	idx = -((-idx) << SHIFT) + 1;
    else
	idx = idx << SHIFT ;

    return idx;
}
