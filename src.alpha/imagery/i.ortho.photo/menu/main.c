#include "dba_imagery.h"

static char title[80];
static char *info[] =
{
title,
"",
"This program ortho-rectifies imagery blocks.",
"",
"Please enter the block to be rectified",
0
};


main(argc,argv) char *argv[];
{
    char block[30], subblock[30], buf[80];
    int new;

    G_gisinit (argv[0]);


/***
    I_location_info (title, argv[0]);
    while(1)
    {
	if (!I_vask_block_new (info, block, "EXIT"))
	    break;
	new = !I_find_block (block);
	if (new)
	{
	    sprintf (buf,"\n%s - does not exist, ", block);
	    G_warning (buf);
	    continue;
	}
        if (!I_vask_subblock_old (info, block, subblock, 0, ""))
	    break;
****/
	menu (block, subblock);
    }
}



