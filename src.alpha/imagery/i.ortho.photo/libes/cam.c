/***********************************************************
* I_fopen_group_camera_new (group)
* I_fopen_group_camera_old (group)
*
* fopen() the imagery group camera reference file "CAMERA"
* (containing the name of the camera associated with the group)
**********************************************************/
#include "orthophoto.h"

FILE *
I_fopen_group_camera_new (group)
    char *group;
{
    return I_fopen_group_file_new (group, "CAMERA");
}

FILE *
I_fopen_group_camera_old (group)
    char *group;
{
    return I_fopen_group_file_old (group, "CAMERA");
}


