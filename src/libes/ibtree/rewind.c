#include "ibtree.h"

int ibtree_rewind (IBTREE *B)
{
    B->cur = 0;

    return 0;
}
