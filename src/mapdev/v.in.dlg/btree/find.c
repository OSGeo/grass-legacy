#include <stdio.h>
#include "btree.h"

int btree_find (B, key, data)
    BTREE *B;
    int key;
    int *data;
{
    register int q;
    int (*cmp)();
    int dir;


    if (B->N <= 0)
	return 0;

    cmp = B->cmp;

    q = 1;
    while (q > 0)
    {
	dir = (*cmp)(B->node[q].key, key) ;
	if (dir == 0)
	{
	    *data = B->node[q].data;
	    return 1;
	}
	if (dir > 0)
	    q = B->node[q].left;             /* go left */
	else
	    q = B->node[q].right;            /* go right */
    }

    return 0;
}
