#include "dba_imagery.h"

menu (block, subblock)
    char *block;
    char *subblock;
{
    char title[80];
    char buf[128];
    int nfiles, subnfiles;
    struct Block_Image_Group_Ref Block_Image_Group_Ref;
    struct Block_Image_Group_Ref Sub_Block_Image_Group_Ref;

    if (!I_find_block (block))
	return;
    if (!I_find_subblock (block,subblock))
	return;
    while (1)
    {
        if (!I_get_subblock (block,subblock))
	   return;

        sprintf (buf, "BLOCK: %s SUB-BLK: %s", block, subblock);
        I_location_info (title, buf);
	I_get_block_Block_Image_Group_Ref (block, &Block_Image_Group_Ref);
	nfiles = Block_Image_Group_Ref.nfiles;

	I_free_block_Block_Image_Group_Ref (&Block_Image_Group_Ref);

	I_get_subblock_Block_Image_Group_Ref (block, subblock, &Sub_Block_Image_Group_Ref);
	subnfiles = Sub_Block_Image_Group_Ref.nfiles;
	I_free_block_Block_Image_Group_Ref (&Sub_Block_Image_Group_Ref);

	G_clear_screen();
	fprintf (stderr, "%s\n\n", title);

	fprintf (stderr, "       1.     Select another sub-block\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "       2.     Compute image-to-photo transformation\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "       3.     Initialize exposure station parameters\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "       4.     Compute ortho-rectification parametrs\n");
	fprintf (stderr, "\n");
/****
	fprintf (stderr, "       5.     Select target window\n");
	fprintf (stderr, "\n");
****/
	fprintf (stderr, "       5.     Ortho-rectify imagery files\n");
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
	    run ("choosesub", block, subblock);
	if (strcmp (buf, "2") == 0)
	    run ("i.block.ref", block, subblock);
	if (strcmp (buf, "3") == 0)
	    run ("i.block.init", block, subblock);
	if (strcmp (buf, "4") == 0)
	    run ("i.block.cont", block, subblock);
/**
	if (strcmp (buf, "5") == 0)
	    run ("i.block.control", block, subblock);
**/
	else if (strcmp (buf, "5") == 0)
	    run ("i.block.rect", block, subblock);
    }
}

