

/**********************************************************
* I_get_group (group);
* I_put_group (group);
*
* I_get_group_ref (group, &Ref);
* I_put_group_ref (group, &Ref);
* I_get_subgroup_ref_file (group, subgroup, &Ref);
* I_put_subgroup_ref_file (group, subgroup, &Ref);
* I_add_file_to_group_ref (name, mapset, &Ref)
* I_transfer_group_ref_file (&Src_ref, n, &Dst_ref)
* I_init_group_ref (&Ref);
* I_free_group_ref (&Ref);
**********************************************************/
#include <string.h>
#include <stdlib.h>
#include "imagery.h"
static int get_ref(char *,char *,struct Ref *);
static int set_color(char *,char *,char *,struct Ref *);
static int put_ref(char *,char *,struct Ref *);

/* get current group name from file GROUPFILE in current mapset */
int I_get_group(char *group)
{
    FILE *fd;
    int stat;

    *group = 0;
    G_suppress_warnings(1);
    fd = G_fopen_old ("", GROUPFILE, G_mapset());
    G_suppress_warnings(0);
    if (fd == NULL)
	return 0;
    stat = (fscanf (fd, "%s", group) == 1);
    fclose (fd);
    return stat;
}

/* write group name to file GROUPFILE in current mapset */
int I_put_group(char *group)
{
    FILE *fd;

    fd = G_fopen_new ("", GROUPFILE);
    if (fd == NULL)
	return 0;
    fprintf (fd, "%s\n", group);
    fclose (fd);
    return 1;
}

/* get current subgroup for group in current mapset */
int I_get_subgroup (char *group,char *subgroup)
{
    FILE *fd;
    FILE *I_fopen_group_file_old();
    int stat;

    *subgroup = 0;
    if (!I_find_group(group))
	return 0;
    G_suppress_warnings(1);
    fd = I_fopen_group_file_old (group, SUBGROUPFILE);
    G_suppress_warnings(0);
    if (fd == NULL)
	return 0;
    stat = (fscanf (fd, "%s", subgroup) == 1);
    fclose (fd);
    return stat;
}

/* write current subgroup to group in current mapset */
int I_put_subgroup(char *group,char *subgroup)
{
    FILE *fd;
    FILE *I_fopen_group_file_new();

    if (!I_find_group(group))
	return 0;
    fd = I_fopen_group_file_new (group, SUBGROUPFILE);
    if (fd == NULL)
	return 0;
    fprintf (fd, "%s\n", subgroup);
    fclose (fd);
    return 1;
}

int I_get_group_ref(
    char *group,
    struct Ref *ref)
{
    return get_ref (group, "", ref);
}

int I_get_subgroup_ref(
    char *group,
    char *subgroup,
    struct Ref *ref)
{
    return get_ref (group, subgroup, ref);
}

static int get_ref (
    char *group,
    char *subgroup,
    struct Ref *ref)
{
    int n;
    char buf[200];
    char name[30], mapset[30];
    char color[20];
    FILE *fd;
    FILE *I_fopen_group_ref_old();
    FILE *I_fopen_subgroup_ref_old();

    I_init_group_ref (ref);

    G_suppress_warnings(1);
    if (*subgroup == 0)
	fd = I_fopen_group_ref_old(group) ;
    else
	fd = I_fopen_subgroup_ref_old(group,subgroup) ;
    G_suppress_warnings(0);
    if (!fd) return 0;

    while (fgets(buf, sizeof buf, fd))
    {
	n=sscanf (buf, "%29s %29s %15s", name, mapset, color);
	if (n==2 || n==3)
	{
	    I_add_file_to_group_ref (name, mapset, ref);
	    if (n==3)
		set_color (name, mapset, color, ref);
	}
    }
/* make sure we have a color assignment */
    I_init_ref_color_nums (ref);

    fclose (fd);
    return 1;
}

static int set_color(char *name,char *mapset,char *color,struct Ref *ref)
{
    int n;

    for (n = 0; n < ref->nfiles; n++)
    {
	if (strcmp (ref->file[n].name, name) == 0
	&&  strcmp (ref->file[n].mapset, mapset) == 0)
		break;
    }

    if (n < ref->nfiles)
	while (*color)
	{
	    switch (*color)
	    {
	    case 'r': case 'R': if(ref->red.n < 0) ref->red.n = n; break;
	    case 'g': case 'G': if(ref->grn.n < 0) ref->grn.n = n; break;
	    case 'b': case 'B': if(ref->blu.n < 0) ref->blu.n = n; break;
	    }
	    color++;
	}

	return 0;
}

int I_init_ref_color_nums(struct Ref *ref)
{
    ref->red.table = NULL;
    ref->grn.table = NULL;
    ref->blu.table = NULL;

    ref->red.index = NULL;
    ref->grn.index = NULL;
    ref->blu.index = NULL;

    if (ref->nfiles <= 0 || ref->red.n >= 0 || ref->blu.n >= 0 || ref->blu.n >= 0)
	return 1;
    switch (ref->nfiles)
    {
    case 1:
	ref->red.n = 0;
	ref->grn.n = 0;
	ref->blu.n = 0;
	break;
    case 2:
	ref->blu.n = 0;
	ref->grn.n = 1;
	break;
    case 3:
	ref->blu.n = 0;
	ref->grn.n = 1;
	ref->red.n = 2;
	break;
    case 4:
	ref->blu.n = 0;
	ref->grn.n = 1;
	ref->red.n = 3;
	break;
    default:
	ref->blu.n = 1;
	ref->grn.n = 2;
	ref->red.n = 4;
	break;
    }

	return 0;
}

int I_put_group_ref(char *group, struct Ref *ref)
{
    return put_ref (group, "", ref);
}

int I_put_subgroup_ref(char *group, char *subgroup, struct Ref *ref)
{
    return put_ref (group, subgroup, ref);
}

static int put_ref( char *group, char *subgroup, struct Ref *ref)
{
    int n;
    FILE *fd;
    FILE *I_fopen_group_ref_new();
    FILE *I_fopen_subgroup_ref_new();


    if (*subgroup == 0)
	fd = I_fopen_group_ref_new(group) ;
    else
	fd = I_fopen_subgroup_ref_new(group,subgroup) ;
    if (!fd) return 0;

    for (n=0; n < ref->nfiles; n++)
    {
	fprintf (fd, "%s %s", ref->file[n].name, ref->file[n].mapset);
	if (n == ref->red.n || n == ref->grn.n || n == ref->blu.n)
	{
	    fprintf (fd, " ");
	    if (n == ref->red.n)
		fprintf (fd,"r");
	    if (n == ref->grn.n)
		fprintf (fd,"g");
	    if (n == ref->blu.n)
		fprintf (fd,"b");
	}
	fprintf (fd, "\n");
    }
    fclose (fd);
    return 1;
}

int I_add_file_to_group_ref(char *name, char *mapset, struct Ref *ref)
{
    int n;

    for (n = 0; n < ref->nfiles; n++)
    {
	if (strcmp (ref->file[n].name, name) == 0
	&&  strcmp (ref->file[n].mapset, mapset) == 0)
		return n;
    }

    if ((n = ref->nfiles++))
	ref->file = (struct Ref_Files *) G_realloc (ref->file, ref->nfiles * sizeof (struct Ref_Files));
    else
	ref->file = (struct Ref_Files *) G_malloc (ref->nfiles * sizeof (struct Ref_Files));
    strcpy (ref->file[n].name, name);
    strcpy (ref->file[n].mapset, mapset);
    return n;
}

int I_transfer_group_ref_file (struct Ref *ref2, int n, struct Ref *ref1)
{
    int k;

/* insert old name into new ref */
    k = I_add_file_to_group_ref(ref2->file[n].name, ref2->file[n].mapset, ref1);

/* preserve color assignment */
    if (n == ref2->red.n)
	ref1->red.n = k;
    if (n == ref2->grn.n)
	ref1->grn.n = k;
    if (n == ref2->blu.n)
	ref1->blu.n = k;

	return 0;
}


int I_init_group_ref( struct Ref *ref)
{
    ref->nfiles = 0;
    ref->red.n = ref->grn.n = ref->blu.n = -1;
    ref->red.table = ref->grn.table = ref->blu.table = NULL;

	return 0;
}

int I_free_group_ref(struct Ref *ref)
{
    if (ref->nfiles > 0)
	free (ref->file);
    ref->nfiles = 0;

	return 0;
}
