/***********************************************************
* I_fopen_block_elev_new (block)
* I_fopen_block_elev_old (block)
*
* fopen() the imagery block elev reference file "ELEV"
* (containing the name of the elev associated with the block)
**********************************************************/
#include "dba_imagery.h"

FILE *
I_fopen_block_elev_new (block)
    char *block;
{
    return I_fopen_block_file_new (block, "ELEV");
}

FILE *
I_fopen_block_elev_old (block)
    char *block;
{
    return I_fopen_block_file_old (block, "ELEV");
}








