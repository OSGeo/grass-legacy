#include <stdio.h>
#include "ibtree.h"


ibtree_create (B, cmp, incr)
    IBTREE *B;
    int (*cmp)();
{
    char *malloc();

    if (incr <= 0)
	incr = 1;

    B->N = 0;
    B->cur = 0;
    B->tlen = B->incr = incr;

/* must have at least 2 nodes, since node[0] is never used */
    if (B->tlen == 1)
	B->tlen = 2;

    B->cmp = cmp;
    B->node = (IBTREE_NODE *) malloc (B->tlen * sizeof (IBTREE_NODE));
    if (B->node == NULL)
	return 0;
    return 1;
}
