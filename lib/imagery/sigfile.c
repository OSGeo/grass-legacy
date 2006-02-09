#include <string.h>
#include <grass/imagery.h>

static int lister(char *,char *,char *);
static char *g;
static char *sg;
static int nfiles;

int I_ask_signature_file_any (
    char *prompt,
    char *group,
    char *subgroup,
    char *name)
{
    char element[200];
    char desc[100];

    sprintf (element, "group/%s/subgroup/%s/sig", group, subgroup);

    sprintf (desc,"subgroup [%s] signature", subgroup);
    g = group;	/* for lister */
    sg = subgroup;
    nfiles = I_number_of_subgroup_ref_files (group, subgroup);
    return G_ask_any_ext(prompt, name, element, desc, 1, "with titles", lister) != NULL;
}

int I_ask_signature_file_old (
    char *prompt,
    char *group,
    char *subgroup,
    char *name)
{
    char element[200];
    char desc[100];

    sprintf (element, "group/%s/subgroup/%s/sig", group, subgroup);

    sprintf (desc,"subgroup [%s] signature", subgroup);
    g = group;	/* for lister */
    sg = subgroup;	/* for lister */
    nfiles = I_number_of_subgroup_ref_files (group, subgroup);
    return G_ask_in_mapset_ext (prompt, name, element, desc, "with titles", lister) != NULL;
}

FILE *I_fopen_signature_file_new (
    char *group,
    char *subgroup,
    char *name)
{
    char element[200];
    FILE *fd;

    sprintf (element, "group/%s/subgroup/%s/sig", group, subgroup);

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

FILE *I_fopen_signature_file_old(
    char *group,
    char *subgroup,
    char *name)
{
    char element[200];
    FILE *fd;

    sprintf (element, "group/%s/subgroup/%s/sig", group, subgroup);

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

static int lister(char *name,char *mapset,char *buf)
{
    struct Signature S;
    FILE *fd;
    int stat;

    *buf = 0;
    if (*name == 0) return 0;

    I_init_signatures (&S,nfiles);
    fd = I_fopen_signature_file_old (g, sg, name);
    if (fd == NULL) return 0;

    stat = I_read_signatures (fd, &S);
    fclose (fd);

    if (stat > 0)
    {
	char temp[200];

	sprintf (temp, "%s(%d signature%s)", S.title,
	    S.nsigs, S.nsigs == 1 ? "" : "s");
	temp[60]=0;
	strcpy (buf, temp);
    }
    else
	strcpy (buf, "** invalid **");
    I_free_signatures (&S);

    return 0;
}
