#include "dba_imagery.h"

static char title[80];
static char *info[] =
{
title,
"",
"This program edits blocks of imagery groups for use in ortho rectification.",
"You may create and select blocks of imagery groups.",
"You may edit blocks of existing imagery groups.",
"You may create sub-blocks of imagery groups within the block.",
"You may select a TARGET location for the ortho-rectified blocks.",
"You may select an elevation data layer to be used in the TARGET location",
"You may select a camera data file for the block.",
"",
"Please enter the block to be created/modified",
0
};


main(argc,argv) char *argv[];
{
    char block[30];
    int new;

    G_gisinit (argv[0]);

    I_location_info (title, argv[0]);
    while(1)
    {
	if (!I_vask_block_new (info, block, "EXIT"))
	    break;
	new = !I_find_block (block);
	if (new)
	{
	    printf ("\n%s - does not exist, ", block);
	    if(!G_yes("do you wish to create a new block? ", 0))
		continue;
	}
	menu (block, new);
    }
}




