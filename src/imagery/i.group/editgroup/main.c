
#include "imagery.h"

main(argc,argv) char *argv[];
{
    char *group;
    struct Ref ref0, ref1, ref2;
    char *list;
    FILE *fd;
    int n;
    int new;

    if (argc != 2)
    {
	fprintf (stderr, "Usage: %s group\n", argv[0]);
	exit(1);
    }

    G_gisinit (argv[0]);
    group = argv[1];

    if (G_legal_filename (group) < 0)
    {
	fprintf (stderr, "`%s' - illegal group name\n", group);
	exit(1);
    }
    list = G_tempfile();


    new = !I_find_group (group);

/*
 * get the current list for files in the group
 */
    I_get_group_ref (group, &ref0);
    I_get_group_ref (group, &ref2);
    while(1)
    {
/*
 * copy current list to ref1
 */
	I_init_group_ref (&ref1);
	for (n=0; n < ref2.nfiles; n++)
	    I_transfer_group_ref_file (&ref2, n, &ref1);
	I_free_group_ref (&ref2);
/*
 * prepre a list of all cellfiles not in the group into a temp file
 */
	unlink (list);
	fd = fopen (list, "w");
	if (fd == NULL)
	    G_fatal_error ("Can't open any temp files");
	find_all_cellfiles (fd, &ref1);
	fclose (fd);
	fd = fopen (list, "r");
	if (fd == NULL)
	    G_fatal_error ("Can't open any temp files");
/*
 * ask for files to be deleted from current list
 */
	I_init_group_ref (&ref2);
	if (ref1.nfiles > 0)
	{
	    if(!ask_oldfiles (&ref1, &ref2, group))
		goto CANCEL;
	    I_free_group_ref (&ref1);
	}
/*
 * ask for files to be added to group
 */
	if(!ask_newfiles (fd, &ref2, group))
	    goto CANCEL;
	fclose (fd);
	unlink (list);
/*
 * display list to user and ask for verification
 */
	if (ref0.nfiles == ref2.nfiles)
	{
	    for (n=0; n < ref0.nfiles; n++)
	    {
		if (strcmp (ref0.file[n].name, ref2.file[n].name))
			break;
		if (strcmp (ref0.file[n].mapset, ref2.file[n].mapset))
			break;
	    }
	    if (n == ref0.nfiles)
	    {
		if (new)
		    printf ("No files selected, group [%s] not created\n", group);
		else
		    printf ("No changes were made to group [%s]\n", group);
		exit(1);
	    }
	}
	printf ("Group [%s] ", group);
	if (ref2.nfiles <= 0)
	    printf ("contains no files\n");
	else
	{
	    printf ("references the following raster file%s\n", ref2.nfiles==1?"":"s");
	    printf ("-----------------------\n");
	    for (n = 0; n < ref2.nfiles; n++)
		printf ("\t%s in %s\n", ref2.file[n].name, ref2.file[n].mapset);
	}
	printf ("-----------------------\n");
	if (G_yes("Look ok? ",-1))
	    break;
    }
    if (I_put_group_ref (group, &ref2))
    {
	if (new)
	    printf ("Group [%s] created!\n", group);
	else
	    printf ("Group [%s] updated!\n", group);
	exit (1);
    }
CANCEL:
    if (new)
	printf ("Group [%s] not created\n", group);
    else
	printf ("Group [%s] not updated\n", group);
    exit(1);
}
