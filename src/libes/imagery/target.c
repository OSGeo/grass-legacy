#include "imagery.h"

int I_get_target(
    char *group,
    char *location,
    char *mapset)
{
    FILE *fd;
    int ok;

    *location = *mapset = 0;
    G_suppress_warnings (1);
    fd = I_fopen_group_file_old (group, "TARGET");
    G_suppress_warnings (0);
    if (fd == NULL)
	return 0;
    ok = (fscanf (fd, "%s %s", location, mapset) == 2);
    fclose (fd);
    if (!ok)
    {
	char msg[100];
	*location = *mapset = 0;
	sprintf (msg, "unable to read target file for group [%s]", group);
	G_warning (msg);
    }
    return ok;
}

int I_put_target (
    char *group,
    char *location,
    char *mapset)
{
    FILE *fd;

    fd = I_fopen_group_file_new (group, "TARGET");
    if (fd == NULL)
	return 0;
    fprintf (fd, "%s\n%s\n", location, mapset);
    fclose (fd);
    return 1;
}
