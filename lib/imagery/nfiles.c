/*************************************************************
* I_number_of_group_ref_files (group)
* I_number_of_subgroup_ref_files (group, subgroup)
*************************************************************/
#include "imagery.h"
#include "gis.h"
#include <stdio.h>

static int nfiles(char *,char *);

int I_number_of_group_ref_files (char *group)
{
    return nfiles(group,"");
}

int I_number_of_subgroup_ref_files (
    char *group,
    char *subgroup)
{
    return nfiles(group,subgroup);
}

static int nfiles(char *group,char *subgroup)
{
    FILE *fd;
    FILE *I_fopen_group_ref_old();
    FILE *I_fopen_subgroup_ref_old();
    int n;
    char buf[1024];
    char name[INAME_LEN], mapset[INAME_LEN];

    G_suppress_warnings(1);
    n = 0;
    if (*subgroup == 0)
	fd = I_fopen_group_ref_old (group);
    else
	fd = I_fopen_subgroup_ref_old (group, subgroup);
    G_suppress_warnings(0);

    if (fd)
    {
	while (fgets(buf, sizeof buf, fd))
	    if (sscanf (buf, "%s %s", name, mapset) == 2)
		n++;
	fclose (fd);
    }

    return n;
}
