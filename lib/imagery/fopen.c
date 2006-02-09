#include <grass/imagery.h>
#include <grass/gis.h>
/******************************************************
* I_fopen_group_file_new()
* I_fopen_group_file_append()
* I_fopen_group_file_old()
*
* fopen new group files in the current mapset
* fopen old group files anywhere
*******************************************************/

static int error  (char *group, char *file, char *msga, char *msgb)

{
    char buf[100];
    sprintf (buf, "%sfile [%s] of group [%s in %s]%s",
	msga, file, group, G_mapset(), msgb);
    G_debug(4,"imagery lib fopen error: %s", buf);
    G_warning (buf);

    return 0;
}

static int error2  (char *group, char *subgroup, char *file, char *msga, char *msgb)

{
    char buf[200];
    sprintf (buf, "%sfile [%s] for subgroup [%s] of group [%s in %s]%s",
	msga, file, subgroup, group, G_mapset(), msgb);
    G_warning (buf);

    return 0;
}

FILE *
I_fopen_group_file_new(
    char *group,
    char *file)
{
    FILE *fd;
    char element[100];

/* get group element name */
    sprintf (element, "group/%s", group);

    fd = G_fopen_new (element, file);
    if (!fd)
	error (group, file, "can't create ", "");
    return fd;
}

FILE *
I_fopen_group_file_append (
    char *group,
    char *file)
{
    FILE *fd;
    char element[100];

/* get group element name */
    sprintf (element, "group/%s", group);

    fd = G_fopen_append (element, file);
    if (!fd)
	error (group, file, "unable to open ", "");
    return fd;
}

FILE *
I_fopen_group_file_old (
    char *group,
    char *file)
{
    FILE *fd;
    char element[100];

/* find file first */
    if (!I_find_group_file (group, file))
    {
	error (group, file, "", " not found");
	return ((FILE *) NULL);
    }

/* get group element name */
    sprintf (element, "group/%s", group);

    fd = G_fopen_old (element, file, G_mapset());
    if (!fd)
	error (group, file, "can't open ", "");
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
	error2 (group, subgroup, file, "can't create ", "");
    return fd;
}

FILE *
I_fopen_subgroup_file_append(
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
	error2 (group, subgroup, file, "unable to open ", "");
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
	error2 (group, subgroup, file, "", " not found");
	return ((FILE *) NULL);
    }

/* get subgroup element name */
    sprintf (element, "group/%s/subgroup/%s", group, subgroup);

    fd = G_fopen_old (element, file, G_mapset());
    if (!fd)
	error2 (group, subgroup, file, "can't open ", "");
    return fd;
}
