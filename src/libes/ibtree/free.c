#include "ibtree.h"

ibtree_free (B)
    IBTREE *B;
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
}
