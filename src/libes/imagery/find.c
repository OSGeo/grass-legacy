/**************************************************************
* I_find_group (group)
*
* Find the a group in the current mapset
**************************************************************/
#include "imagery.h"
#include "gis.h"


/*!
 * \brief does group exist?
 *
 * Returns 1 if the
 * specified <b>group</b> exists in the current mapset; 0 otherwise.
 *
 *  \param group
 *  \return int
 */

int I_find_group(char *group)
{
    if (group == NULL || *group == 0)
	return 0;

    return G_find_file2 ("group", group, G_mapset()) != NULL ;
}

int I_find_group_file(char *group, char *file)
{
    char element[100];

    if (!I_find_group (group))
	return 0;
    if (file == NULL || *file == 0)
	return 0;

    sprintf (element, "group/%s", group);

    return G_find_file (element, file, G_mapset()) != NULL ;
}

int I_find_subgroup(char *group,char *subgroup)
{
    char element[300];

    if (!I_find_group(group))
	return 0;
    if (subgroup == NULL || *subgroup == 0)
	return 0;

    sprintf (element, "group/%s/subgroup", group);

    return G_find_file (element, subgroup, G_mapset()) != NULL ;
}

int I_find_subgroup_file( char *group, char *subgroup, char *file)
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
