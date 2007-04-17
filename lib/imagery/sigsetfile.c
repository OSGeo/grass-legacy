#include <grass/imagery.h>

FILE *I_fopen_sigset_file_new( char *group, char *subgroup, char *name)
{
    char element[200];
    FILE *fd;

    sprintf (element, "group/%s/subgroup/%s/sigset", group, subgroup);

    fd = G_fopen_new (element, name);
    if (fd == NULL)
    {
	char msg[200];
	sprintf (msg, "unable to create signature file %s for subgroup %s of group %s",
		name, subgroup, group);
	G_warning (msg);
    }
    return fd;
}

FILE *I_fopen_sigset_file_old ( char *group, char *subgroup, char *name)
{
    char element[200];
    FILE *fd;

    sprintf (element, "group/%s/subgroup/%s/sigset", group, subgroup);

    fd = G_fopen_old (element, name, G_mapset());
    if (fd == NULL)
    {
	char msg[200];
	sprintf (msg, "unable to open signature file %s for subgroup %s of group [%s in %s]",
		name, subgroup, group, G_mapset());
	G_warning (msg);
    }
    return fd;
}
