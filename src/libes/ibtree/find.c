#include <stdio.h>
#include "ibtree.h"

int ibtree_find (B, key, data)
    IBTREE *B;
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
