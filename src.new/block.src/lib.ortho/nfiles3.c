/*************************************************************
* I_number_of_block_Block_Image_Group_Ref_files (block)
* I_number_of_subblock_Block_Image_Group_Ref_files (block, subblock)
*************************************************************/
#include <stdio.h>

I_number_of_block_Block_Image_Group_Ref_files (block)
    char *block;
{
    return nfiles(block,"");
}

I_number_of_subblock_Block_Image_Group_Ref_files (block, subblock)
    char *block;
    char *subblock;
{
    return nfiles(block,subblock);
}

static
nfiles (block, subblock)
    char *block;
    char *subblock;
{
    FILE *fd;
    FILE *I_fopen_block_Block_Image_Group_Ref_old();
    FILE *I_fopen_subblock_Block_Image_Group_Ref_old();
    int n;
    char buf[1024];
    char name[30], mapset[30];

    G_suppress_warnings(1);
    n = 0;
    if (*subblock == 0)
	fd = I_fopen_block_Block_Image_Group_Ref_old (block);
    else
	fd = I_fopen_subblock_Block_Image_Group_Ref_old (block, subblock);
    G_suppress_warnings(0);

    if (fd)
    {
	while (fgets(buf, sizeof buf, fd))
	    if (sscanf (buf, "%s %s", name, mapset) == 2)
		n++;
	fclose (fd);
    }

    return n;
}



