
#include "dba_imagery.h"

static char title[80];
static char *info[]=
{
title,
"",
"Please enter the subblock to be ortho-rectified",
NULL
};

main(argc,argv) char *argv[];
{
    char *block;
    char *subblock;
    struct Block_Image_Group_Ref Block_Image_Group_Ref;
    char buf[30];

    if (argc != 3)
    {
	fprintf (stderr, "usage: %s block subblock\n", argv[0]);
	exit(1);
    }

    G_gisinit (argv[0]);
    block = argv[1];
    subblock = argv[2];
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
    sprintf (buf, "BLOCK: %s \tSUB-BLOCK: %s", block, subblock);
    I_location_info (title, buf);

    if (!I_vask_subblock_old (info, block, subblock, 0, ""))
	exit(1);
}






