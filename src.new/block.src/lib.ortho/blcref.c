/***********************************************************
* I_fopen_block_Block_Image_Group_Ref_new (block)
* I_fopen_block_Block_Image_Group_Ref_old (block)
*
* fopen() the imagery block Block_Image_Group_Reference file (containing the number
* of files and the names of the cell files which comprise
* the block)
**********************************************************/
#include "dba_imagery.h"

FILE *
I_fopen_block_Block_Image_Group_Ref_new (block)
    char *block;
{
    return I_fopen_block_file_new (block, "Image_Group_Ref");
}

FILE *
I_fopen_block_Block_Image_Group_Ref_old (block)
    char *block;
{
    return I_fopen_block_file_old (block, "Image_Group_Ref");
}

/*
FILE *
I_fopen_block_Block_Image_Group_Ref_append (block)
    char *block;
{
    return I_fopen_block_file_append (block, "Image_Group_Ref");
}
*/

FILE *
I_fopen_subblock_Block_Image_Group_Ref_new (block,subblock)
    char *block;
    char *subblock;
{
    return I_fopen_subblock_file_new (block, subblock, "Image_Group_Ref");
}

FILE *
I_fopen_subblock_Block_Image_Group_Ref_old (block,subblock)
    char *block;
    char *subblock;
{
    FILE *fd;

    fd = I_fopen_subblock_file_old (block, subblock, "Image_Group_Ref");
    return fd;
}



