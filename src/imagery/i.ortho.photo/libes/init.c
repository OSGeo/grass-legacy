/***********************************************************
* I_fopen_group_init_new (group)
* I_fopen_group_init_old (group)
*
* fopen() the imagery group reference file (containing the number
* of files and the names of the cell files which comprise
* the group)
**********************************************************/
#include "imagery.h"

FILE *
I_fopen_group_init_new (group)
    char *group;
{
    return I_fopen_group_file_new (group, "INIT_EXP");
}

FILE *
I_fopen_group_init_old (group)
    char *group;
{
    return I_fopen_group_file_old (group, "INIT_EXP");
}

