#include "global.h"

static char title[80];
char *info[]=
{
title,
"",
"Please select the group/subgroup containing the signatures",
"to be used in the classification",
NULL };

ask_files (me) char *me;
{
    char *name, *mapset;
    int hitreturn;
    char prompt[100];

    FILE *fd;
    int n;
    int any;

    I_location_info (title, "MAXIMUM LIKELIHOOD");
    hitreturn = 0;
    I_init_group_ref (&Ref);
    while(1)
    {
	if (hitreturn)
	{
	    char buf[100];
	    printf ("Hit RETURN -->");
	    G_gets(buf);
	}
	hitreturn = 1;
	if(!I_vask_subgroup_old (info, group, subgroup, 1, ""))
	    exit(0);
	I_free_group_ref (&Ref);
	I_get_subgroup_ref (group, subgroup, &Ref);

	any = 0;
	for (n= 0; n < Ref.nfiles; n++)
	{
	    name = Ref.file[n].name;
	    mapset = Ref.file[n].mapset;
	    if (G_find_cell (name, mapset) == NULL)
	    {
		if (!any)
		    printf ("\7\n** The following raster files in subgroup [%s] do not exist\n", subgroup);
		any = 1;
		printf ("      %s@%s\n", name, mapset);
	    }
	}
	if (any)
	    continue;
	if (Ref.nfiles > 1)
	    break;
	printf ("Subgroup [%s] of group [%s] ", subgroup, group);
	if (Ref.nfiles <= 0)
	    printf ("doesn't have any files\n");
	else
	    printf ("only has 1 file\n");
	printf ("The subgroup must have at least 2 files to run %s\n", me);
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
	{
	    printf ("Unable to proceed\n");
	    sleep(3);
	    exit(1);
	}
    }

    printf ("%s\n", title);
    sprintf (prompt,"Enter the signature file to be used for classification");
    for(;;)
    {
	I_init_signatures (&S, Ref.nfiles);
	printf ("\nSIGNATURE");
	if(!I_ask_signature_file_old (prompt, group, subgroup, sigfile))
	    exit(0);

	fd = I_fopen_signature_file_old (group, subgroup, sigfile);
	if (fd == NULL) continue;
	n = I_read_signatures (fd, &S);
	fclose (fd);
	if (n < 0)
	{
	    printf ("** can't read signature file %s **\n", sigfile);
	    continue;
	}
	if (S.nsigs > 255)
	{
	    printf ("%s has too many signatures\n", sigfile);
	    continue;
	}
	B = (double *) G_malloc (S.nsigs * sizeof (double));
	if (!invert_signatures())
	{
	    printf ("You may continue, but the invalid signatures will not be used\n");
	    if (!G_yes("Continue? ", 0))
	    {
		free (B);
		continue;
	    }
	}
	break;
    }

/* classified layer */
    if (!G_ask_cell_new ("Please name the CLASSIFIED map layer to be generated", class_name))
	exit(0);
    class_cell = G_allocate_cell_buf();

/* reject layer - chi square tests */
    reject_cell = NULL;
    for(;;)
    {
	G_set_ask_return_msg ("if you don't want this layer");
	if (!G_ask_cell_new ("Please name the REJECT THRESHOLD map layer to be generated", reject_name))
	    break;
	if (strcmp(reject_name, class_name) == 0)
	    printf ("\n** Please choose a different name\n\n");
	else
	{
	    reject_cell = G_allocate_cell_buf();
	    break;
	}
    }

    for (n=0; n<S.nsigs;n++)
	if (!S.sig[0].have_color)
		break;

/* if any signature does not already have a color, we will
 * use the colors for 3 of the layers in the subgroup
 * ask for those layers.
 * Also, save the reference list so that the color choices
 * will also be saved.
 */
    if (n < S.nsigs)
    {
	sprintf (title,"COLOR CONFIGURATION FOR CLASSIFIED LAYER [%s]",
	    class_name);
	I_ask_ref_colors (title, &Ref);
	I_put_subgroup_ref (group, subgroup, &Ref);
	have_colors = (I_read_group_colors (group, &Ref) > 0);
    }
}
