#include "dba_imagery.h"

menu (block, new)
    char *block;
{
    char title[80];
    char buf[128];
    int nfiles;
    struct Block_Image_Group_Ref Block_Image_Group_Ref;

    if (new)
	run ("editblock", block);

    if (!I_find_block (block))
	return;

    sprintf (buf, "BLOCK: %s", block);
    I_location_info (title, buf);
    while (1)
    {
	I_get_block_Block_Image_Group_Ref (block, &Block_Image_Group_Ref);
	nfiles = Block_Image_Group_Ref.nfiles;
	I_free_block_Block_Image_Group_Ref (&Block_Image_Group_Ref);

	G_clear_screen();
	fprintf (stderr, "%s\n\n", title);

	fprintf (stderr, "       1.     Select a different block\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "       2.     Edit block title\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "       3.     Select a TARGET location\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "       4.     Select an elevation data layer\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "       5.     Select a camera reference file\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "       6.     Include new imagery groups in the block\n");
	if (nfiles > 0)
	{
	    fprintf (stderr, "              or remove imagery groups from the block\n");
	    fprintf (stderr, "\n");
	    fprintf (stderr, "       7.     Create a new subblock within the block\n");
	}
	fprintf (stderr, "\n");
	fprintf (stderr, "     RETURN   exit\n");
	fprintf (stderr, "\n> ");

	if (!G_gets(buf))
	    continue;
	if (*buf == 0)    /* exit */
	    exit(0);

	G_strip (buf);
	fprintf (stderr, "<%s>\n",buf);

	if (strcmp (buf, "1") == 0)
	    break;
	if (strcmp (buf, "2") == 0)
	    run ("titleblc", block);
	if (strcmp (buf, "3") == 0)
	    run ("blctarget", block);
	if (strcmp (buf, "4") == 0)
	    run ("blcelev", block);
	if (strcmp (buf, "5") == 0)
	    run ("blccamera", block);
	else if (strcmp (buf, "6") == 0)
	    run ("editblock", block);
	else if (nfiles > 0 && strcmp (buf, "7") == 0)
	    run ("subblock", block);
    }
}



