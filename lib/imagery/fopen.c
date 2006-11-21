#include <stdio.h>
#include <grass/imagery.h>
#include <grass/gis.h>
#include <grass/glocale.h>


/******************************************************
* I_fopen_group_file_new()
* I_fopen_group_file_append()
* I_fopen_group_file_old()
*
* fopen new group files in the current mapset
* fopen old group files anywhere
*******************************************************/


FILE *
I_fopen_group_file_new(char *group, char *file)
{
    FILE *fd;
    char element[100];

    /* get group element name */
    sprintf (element, "group/%s", group);

    fd = G_fopen_new (element, file);
    if (!fd)
        G_warning (_("Unable to create file [%s] of group [%s in %s]"),
                    file, group, G_mapset());

    return fd;
}


FILE *
I_fopen_group_file_append (char *group, char *file)
{
    FILE *fd;
    char element[100];

    /* get group element name */
    sprintf (element, "group/%s", group);

    fd = G_fopen_append (element, file);
    if (!fd)
        G_warning (_("Unable to open file [%s] of group [%s in %s]"),
                    file, group, G_mapset());

    return fd;
}


FILE *
I_fopen_group_file_old (char *group, char *file)
{
    FILE *fd;
    char element[100];

    /* find file first */
    if (!I_find_group_file (group, file))
    {
        G_warning (_("Unable to find file [%s] of group [%s in %s]"),
                    file, group, G_mapset());

	return ((FILE *) NULL);
    }

    /* get group element name */
    sprintf (element, "group/%s", group);

    fd = G_fopen_old (element, file, G_mapset());
    if (!fd)
        G_warning (_("Unable to open file [%s] of group [%s in %s]"),
                    file, group, G_mapset());

    return fd;
}


FILE *
I_fopen_subgroup_file_new (
    char *group,
    char *subgroup,
    char *file)
{
    FILE *fd;
    char element[300];

    /* get subgroup element name */
    sprintf (element, "group/%s/subgroup/%s", group, subgroup);

    fd = G_fopen_new (element, file);
    if (!fd)
        G_warning (_("Unable to create file [%s] for subgroup [%s] of group [%s in %s]"),
                    file, subgroup, group, G_mapset());

    return fd;
}


FILE *
I_fopen_subgroup_file_append (
    char *group,
    char *subgroup,
    char *file)
{
    FILE *fd;
    char element[300];

    /* get subgroup element name */
    sprintf (element, "group/%s/subgroup/%s", group, subgroup);

    fd = G_fopen_append (element, file);
    if (!fd)
        G_warning (_("Unable to open file [%s] for subgroup [%s] of group [%s in %s]"),
                    file, subgroup, group, G_mapset());

    return fd;
}


FILE *
I_fopen_subgroup_file_old (
    char *group,
    char *subgroup,
    char *file)
{
    FILE *fd;
    char element[300];

    /* find file first */
    if (!I_find_subgroup_file (group, subgroup, file))
    {
        G_warning (_("Unable to find file [%s] for subgroup [%s] of group [%s in %s]"),
                    file, subgroup, group, G_mapset());

	return ((FILE *) NULL);
    }

    /* get subgroup element name */
    sprintf (element, "group/%s/subgroup/%s", group, subgroup);

    fd = G_fopen_old (element, file, G_mapset());
    if (!fd)
        G_warning (_("Unable to open file [%s] for subgroup [%s] of group [%s in %s]"),
                    file, subgroup, group, G_mapset());

    return fd;
}
