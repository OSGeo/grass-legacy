#include "btree.h"

btree_rewind (B)
    BTREE *B;
{
    B->cur = 0;
}
