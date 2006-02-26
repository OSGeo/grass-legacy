#include <stdlib.h>
#include "global.h"
#include "local_proto.h"

int 
open_files (void)
{
    char *name, *mapset;
    FILE *fd;
    int n;

    I_init_group_ref (&Ref);
    if (!I_find_group(group))
    {
	fprintf (stderr, "group=%s - not found\n", group);
	exit(1);
    }
    if (!I_find_subgroup(group, subgroup))
    {
	fprintf (stderr, "subgroup=%s (of group %s) - not found\n", subgroup, group);
	exit(1);
    }

    I_get_subgroup_ref (group, subgroup, &Ref);

    if (Ref.nfiles <= 1)
    {
	fprintf (stderr, "Subgroup [%s] of group [%s] ", subgroup, group);
	if (Ref.nfiles <= 0)
	    fprintf (stderr, "doesn't have any files\n");
	else
	    fprintf (stderr, "only has 1 file\n");
	fprintf (stderr, "The subgroup must have at least 2 files\n");
	exit(1);
    }

    cell = (CELL **) G_malloc (Ref.nfiles * sizeof (CELL *));
    cellfd = (int *) G_malloc (Ref.nfiles * sizeof (int));
    P = (double *) G_malloc (Ref.nfiles * sizeof (double));
    for (n=0; n < Ref.nfiles; n++)
    {
	cell[n] = G_allocate_cell_buf();
	name = Ref.file[n].name;
	mapset = Ref.file[n].mapset;
	if ((cellfd[n] = G_open_cell_old (name, mapset)) < 0)
	    exit(1);
    }

    I_init_signatures (&S, Ref.nfiles);
    fd = I_fopen_signature_file_old (group, subgroup, sigfile);
    if (fd == NULL) exit(1);
    n = I_read_signatures (fd, &S);
    fclose (fd);
    if (n < 0)
    {
	fprintf (stderr, "Can't read signature file %s\n", sigfile);
	exit(1);
    }
    if (S.nsigs > 255)
    {
	fprintf (stderr, "%s has more than 255 signatures\n", sigfile);
	exit(1);
    }
    B = (double *) G_malloc (S.nsigs * sizeof (double));
    invert_signatures();

    class_fd = G_open_cell_new (class_name);
    if (class_fd < 0)
	exit(1) ;

    class_cell = G_allocate_cell_buf();

    reject_cell = NULL;
    if (reject_name)
    {
	reject_fd = G_open_cell_new (reject_name);
	if (reject_fd < 0)
	    fprintf (stderr, "Unable to create reject layer [%s]", reject_name);
	else
	    reject_cell = G_allocate_cell_buf();
    }

    return 0;
}
