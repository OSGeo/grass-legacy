#include "dba_imagery.h"

main(argc,argv) char *argv[];
{
    char *block;
    struct Block_Image_Group_Ref Block_Image_Group_Ref0, Block_Image_Group_Ref1, Block_Image_Group_Ref2;
    char *list;
    FILE *fd;
    int n;
    int new;

    if (argc != 2)
    {
	fprintf (stderr, "Usage: %s block\n", argv[0]);
	exit(1);
    }

    G_gisinit (argv[0]);
    block = argv[1];

    if (G_legal_filename (block) < 0)
    {
	fprintf (stderr, "`%s' - illegal block name\n", block);
	exit(1);
    }
    list = G_tempfile();


    new = !I_find_block (block);

/*
 * get the current list for files in the block
 */
    I_get_block_Block_Image_Group_Ref (block, &Block_Image_Group_Ref0);
    I_get_block_Block_Image_Group_Ref (block, &Block_Image_Group_Ref2);
    while(1)
    {
/*
 * copy current list to Block_Image_Group_Ref1
 */
	I_init_block_Block_Image_Group_Ref (&Block_Image_Group_Ref1);
	for (n=0; n < Block_Image_Group_Ref2.nfiles; n++)
	    I_transfer_block_Block_Image_Group_Ref_file (&Block_Image_Group_Ref2, n, &Block_Image_Group_Ref1);
	I_free_block_Block_Image_Group_Ref (&Block_Image_Group_Ref2);
/*
 * prepre a list of all imagery group files not in the block into a temp file
 */
	unlink (list);
	fd = fopen (list, "w");
	if (fd == NULL)
	    G_fatal_error ("Can't open any temp files");
	find_all_groupfiles (fd, &Block_Image_Group_Ref1);
	fclose (fd);
	fd = fopen (list, "r");
	if (fd == NULL)
	    G_fatal_error ("Can't open any temp files");
/*
 * ask for files to be deleted from current list
 */
	I_init_block_Block_Image_Group_Ref (&Block_Image_Group_Ref2);
	if (Block_Image_Group_Ref1.nfiles > 0)
	{
	    if(!ask_oldfiles (&Block_Image_Group_Ref1, &Block_Image_Group_Ref2, block))
		goto CANCEL;
	    I_free_block_Block_Image_Group_Ref (&Block_Image_Group_Ref1);
	}
/*
 * ask for files to be added to block
 */
	if(!ask_newfiles (fd, &Block_Image_Group_Ref2, block))
	    goto CANCEL;
	fclose (fd);
	unlink (list);
/*
 * display list to user and ask for verification
 */
	if (Block_Image_Group_Ref0.nfiles == Block_Image_Group_Ref2.nfiles)
	{
	    for (n=0; n < Block_Image_Group_Ref0.nfiles; n++)
	    {
		if (strcmp (Block_Image_Group_Ref0.file[n].name, Block_Image_Group_Ref2.file[n].name))
			break;
		if (strcmp (Block_Image_Group_Ref0.file[n].mapset, Block_Image_Group_Ref2.file[n].mapset))
			break;
	    }
	    if (n == Block_Image_Group_Ref0.nfiles)
	    {
		if (new)
		    printf ("No files selected, block [%s] not created\n", block);
		else
		    printf ("No changes were made to block [%s]\n", block);
		exit(1);
	    }
	}
	printf ("Block [%s] ", block);
	if (Block_Image_Group_Ref2.nfiles <= 0)
	    printf ("contains no files\n");
	else
	{
	    printf ("Block_Image_Group_References the following imagery group file%s\n", Block_Image_Group_Ref2.nfiles==1?"":"s");
	    printf ("-----------------------\n");
	    for (n = 0; n < Block_Image_Group_Ref2.nfiles; n++)
		printf ("\t%s in %s\n", Block_Image_Group_Ref2.file[n].name, Block_Image_Group_Ref2.file[n].mapset);
	}
	printf ("-----------------------\n");
	if (G_yes("Look ok? ",-1))
	    break;
    }
    if (I_put_block_Block_Image_Group_Ref (block, &Block_Image_Group_Ref2))
    {
	if (new)
	    printf ("Block [%s] created!. Remeber to create a subblock!\n", block);
	else
	    printf ("Block [%s] updated!  Remeber to check the sub-blocks\n", block);
	exit (1);
    }
CANCEL:
    if (new)
	printf ("Block [%s] not created\n", block);
    else
	printf ("Block [%s] not updated\n", block);
    exit(1);
}



