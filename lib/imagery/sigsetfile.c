#include <grass/imagery.h>

int I_ask_sigset_file_any(
    char *prompt, char *group, char *subgroup, char *name)
{
    char element[200];
    char desc[100];

    sprintf (element, "group/%s/subgroup/%s/sigset", group, subgroup);

    sprintf (desc,"subgroup [%s] signature", subgroup);
    return G_ask_any (prompt, name, element, desc, 1) != NULL;
}

int I_ask_sigset_file_old(
       char *prompt, char *group, char *subgroup, char *name)
{
    char element[200];
    char desc[100];

    sprintf (element, "group/%s/subgroup/%s/sigset", group, subgroup);

    sprintf (desc,"subgroup [%s] signature", subgroup);
    return G_ask_in_mapset (prompt, name, element, desc) != NULL;
}

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
