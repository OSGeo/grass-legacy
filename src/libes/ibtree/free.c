#include <stdlib.h>
#include "ibtree.h"

int ibtree_free (IBTREE *B)
{
    int data, key;

    ibtree_rewind (B);
    while (ibtree_next (B, &key, &data))
    {
	/*
	free (key);
	free (data);
	*/
    }
    free (B->node);

    return 0;
}
