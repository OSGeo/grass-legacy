#include "btree.h"

btree_free (B)
    BTREE *B;
{
    int data, key;

    btree_rewind (B);
    while (btree_next (B, &key, &data))
    {
	/*
	free (key);
	free (data);
	*/
    }
    free (B->node);
}
