
#include "dba_imagery.h"

static char title[80];
static char *info[]=
{
title,
"",
"Please enter the subblock to be created",
NULL
};

main(argc,argv) char *argv[];
{
    char *block;
    char subblock[30];
    struct Block_Image_Group_Ref Block_Image_Group_Ref;

    if (argc != 2)
    {
	fprintf (stderr, "usage: %s block\n", argv[0]);
	exit(1);
    }

    G_gisinit (argv[0]);
    block = argv[1];
    if (!I_find_block (block))
    {
	fprintf (stderr, "** Block [%s] not found\n", block);
	exit(1);
    }

    I_get_block_Block_Image_Group_Ref (block, &Block_Image_Group_Ref);
    if (Block_Image_Group_Ref.nfiles <= 0)
    {
	fprintf (stderr, "** Block [%s] contains no files. Can't form any subblocks\n", block);
	exit(1);
    }
    I_location_info (title, "");
    if (I_vask_subblock_new (info, block, subblock, 0, ""))
	make_subblock (block, subblock, &Block_Image_Group_Ref);
    exit(0);
}




