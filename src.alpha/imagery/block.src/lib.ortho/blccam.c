/***********************************************************
* I_fopen_block_camera_new (block)
* I_fopen_block_camera_old (block)
*
* fopen() the imagery block camera reference file "CAMERA"
* (containing the name of the camera associated with the block)
**********************************************************/
#include "dba_imagery.h"

FILE *
I_fopen_block_camera_new (block)
    char *block;
{
    return I_fopen_block_file_new (block, "CAMERA");
}

FILE *
I_fopen_block_camera_old (block)
    char *block;
{
    return I_fopen_block_file_old (block, "CAMERA");
}






