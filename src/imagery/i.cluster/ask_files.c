#include "global.h"
char title[80];
static char *info[]=
{
title,
"",
"Please select the group/subgroup to be analyzed",
NULL
};
ask_files (me) char *me;
{
    char *name, *mapset;
    FILE *fd;
    int n, any;
    int hitreturn;

    I_location_info (title, "CLUSTER");
    hitreturn = 0;
    I_init_group_ref (&ref);
    while(1)
    {
	if (hitreturn)
	{
	    char buf[100];
	    printf ("Hit RETURN -->");
	    G_gets(buf);
	}
	hitreturn = 1;
	if (!I_vask_subgroup_old (info, group, subgroup, 1, ""))
	    exit(0);
	I_free_group_ref (&ref);
	I_get_subgroup_ref (group, subgroup, &ref);

	any = 0;
	for (n = 0; n < ref.nfiles; n++)
	{
	    name = ref.file[n].name;
	    mapset = ref.file[n].mapset;
	    if (G_find_cell (name, mapset) == NULL)
	    {
		if (!any)
		    printf ("\7\n** The following raster files in subgroup [%s] do not exist\n", subgroup);
		any = 1;
		printf ("       %s@%s\n", name, mapset);
	    }
	}
	if (any) continue;
	if (ref.nfiles > 1)
	    break;
	printf ("Subgroup [%s] ", subgroup);
	if (ref.nfiles <= 0)
	    printf ("doesn't have any files\n");
	else
	    printf ("only has 1 file\n");
	printf ("The subgroup must have at least 2 files to run %s\n", me);
    }

    cell = (CELL **) G_malloc (ref.nfiles * sizeof (CELL *));
    cellfd = (int *) G_malloc (ref.nfiles * sizeof (int));
    for (n=0; n < ref.nfiles; n++)
    {
	cell[n] = G_allocate_cell_buf();
	name   = ref.file[n].name;
	mapset = ref.file[n].mapset;
	if ((cellfd[n] = G_open_cell_old (name, mapset)) < 0)
	{
	    printf ("Unable to proceed\n");
	    exit(1);
	}
    }

    printf ("\nRESULT SIGNATURE");
    if(!I_ask_signature_file_any ("Enter name for the resulting signature file", group, subgroup,  outsigfile))
	exit(0);

    for(;;)
    {
	I_init_signatures (&in_sig, ref.nfiles);
	G_set_ask_return_msg ("to use DEFAULT means");
	printf ("\nSEED SIGNATURES");
	if (!I_ask_signature_file_old(
	    "Select the signature file to use for the initial class means",
	    group, subgroup, insigfile))
	{
	    insigfile[0] = 0;
	    break;
	}
	fd = I_fopen_signature_file_old (group, subgroup, insigfile);
	if (fd == NULL) continue;
	n = I_read_signatures (fd, &in_sig);
	fclose (fd);
	if (n < 0)
	{
	    printf ("** can't read signature file %s **\n", insigfile);
	    continue;
	}
	if (in_sig.nsigs <= 255) break;
	printf ("%s has too many signatures\n", insigfile);
    }
}
