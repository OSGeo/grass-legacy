#include "imagery.h"

transfer_file (Block_Ref2, n, Block_Ref1)
    struct Block_Ref *Block_Ref1, *Block_Ref2;
{
    int k;

/* insert old name into new Block_Ref */
    k = I_add_file_to_group_Block_Ref(Block_Ref2->file[n].name, Block_Ref2->file[n].mapset, Block_Ref1);

}

