/**************************************************************
* I_find_group (group)
*
* Find the a group in the current mapset
**************************************************************/
#include "gis.h"

I_find_group (group)
    char *group;
{
    if (group == NULL || *group == 0)
	return 0;

    return G_find_file2 ("group", group, G_mapset()) != NULL ;
}

I_find_group_file (group, file)
    char *group;
    char *file;
{
    char element[100];

    if (!I_find_group (group))
	return 0;
    if (file == NULL || *file == 0)
	return 0;

    sprintf (element, "group/%s", group);

    return G_find_file (element, file, G_mapset()) != NULL ;
}

I_find_subgroup (group, subgroup)
    char *group;
    char *subgroup;
{
    char element[300];

    if (!I_find_group(group))
	return 0;
    if (subgroup == NULL || *subgroup == 0)
	return 0;

    sprintf (element, "group/%s/subgroup", group);

    return G_find_file (element, subgroup, G_mapset()) != NULL ;
}

I_find_subgroup_file (group, subgroup, file)
    char *group;
    char *subgroup;
    char *file;
{
    char element[300];

    if (!I_find_group(group))
	return 0;
    if (subgroup == NULL || *subgroup == 0)
	return 0;
    if (file == NULL || *file == 0)
	return 0;

    sprintf (element, "group/%s/subgroup/%s", group, subgroup);

    return G_find_file (element, file, G_mapset()) != NULL ;
}
