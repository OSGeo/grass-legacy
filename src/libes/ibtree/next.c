#include "ibtree.h"

ibtree_next (B, key, data)
    IBTREE *B;
    int *key;
    int *data;
{
    int q;

    if (B->N <= 0)
	return 0;

/* if rewound, start at root and go all the way to the left */
    if (B->cur == 0)
	B->cur = 1;
    else			 /* go to the right */
	B->cur = B->node[B->cur].right;

    if (B->cur == 0)          /* no more */
	return 0;

    if (B->cur < 0)           /* thread. stop here */
	B->cur = -(B->cur) ;
    else                       /* go all the way left */
	while (q = B->node[B->cur].left)
	    B->cur = q;

    *key  = B->node[B->cur].key ;
    *data = B->node[B->cur].data;

    return 1;
}
