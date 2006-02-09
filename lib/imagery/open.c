#include <grass/imagery.h>
#include <grass/gis.h>
/******************************************************
* I_open_group_file_new()
* I_open_group_file_old()
*
* open new and old imagery group files in the current mapset
*******************************************************/

static void error  (char *group, char *file, char *msga, char *msgb)

{
    char buf[100];
    sprintf (buf, "%sfile [%s] of group [%s in %s]%s",
	msga, file, group, G_mapset(), msgb);
    G_warning (buf);
}

int I_open_group_file_new ( char *group, char *file)
{
    int fd;
    char element[100];

/* get group element name */
    sprintf (element, "group/%s", group);

    fd = G_open_new (element, file);
    if (fd < 0)
	error (group, file, "can't create ", "");
    return fd;
}

int I_open_group_file_old ( char *group, char *file)
{
    int fd;
    char element[100];

/* find the file first */
    if (!I_find_group_file (group, file))
    {
	error (group, file, "", " not found");
	return -1;
    }

/* get group element name */
    sprintf (element, "group/%s", group);

    fd = G_open_old (element, file, G_mapset());
    if (fd < 0)
	error (group, file, "can't open ", "");
    return fd;
}
