/***********************************************************
* I_fopen_group_elev_new (group)
* I_fopen_group_elev_old (group)
*
* fopen() the imagery group elev reference file "ELEVATION"
* (containing the name of the elev associated with the block)
**********************************************************/
#include "orthophoto.h"

FILE *
I_fopen_group_elev_new (group)
    char *group;
{
    return ((FILE *) I_fopen_group_file_new (group, "ELEVATION"));
}

FILE *
I_fopen_group_elev_old (group)
    char *group;
{
    return ((FILE *) I_fopen_group_file_old (group, "ELEVATION"));
}

I_find_group_elev_file(group)
    char *group;
{
    return I_find_group_file(group, "ELEVATION");
}





