/***********************************************************
* I_fopen_group_ref_new (group)
* I_fopen_group_ref_old (group)
*
* fopen() the imagery group reference file (containing the number
* of files and the names of the cell files which comprise
* the group)
**********************************************************/
#include <grass/imagery.h>

FILE *
I_fopen_group_ref_new (
    char *group)
{
    return I_fopen_group_file_new (group, "REF");
}

FILE *
I_fopen_group_ref_old (
    char *group)
{
    return I_fopen_group_file_old (group, "REF");
}

/*
FILE *
I_fopen_group_ref_append (
    char *group)
{
    return I_fopen_group_file_append (group, "REF");
}
*/

FILE *
I_fopen_subgroup_ref_new (
    char *group,
    char *subgroup)
{
    return I_fopen_subgroup_file_new (group, subgroup, "REF");
}

FILE *
I_fopen_subgroup_ref_old (
    char *group,
    char *subgroup)
{
    FILE *fd;

    fd = I_fopen_subgroup_file_old (group, subgroup, "REF");
    return fd;
}
