
#include "dba_imagery.h"

make_subblock (block, subblock, Block_Image_Group_Ref1)
    char *block;
    char *subblock;
    struct Block_Image_Group_Ref *Block_Image_Group_Ref1;
{
    struct Block_Image_Group_Ref Block_Image_Group_Ref2;
    int n;

    while (1)
    {
	I_init_block_Block_Image_Group_Ref (&Block_Image_Group_Ref2);
	if (!ask_files (block, subblock, Block_Image_Group_Ref1, &Block_Image_Group_Ref2))
	{
	    printf ("Subblock [%s] not created\n", subblock);
	    exit(1);
	}
	if (Block_Image_Group_Ref2.nfiles <= 0)
	{
	    printf ("No files selected! Subblock [%s] not created\n", subblock);
	    exit(1);
	}
	printf ("Subblock [%s] Block_Image_Group_References the following cell file%s\n",
		subblock, Block_Image_Group_Ref2.nfiles==1?"":"s");
	printf ("---------------------------\n");
	for (n = 0; n < Block_Image_Group_Ref2.nfiles; n++)
	    printf ("\t%s in %s\n", Block_Image_Group_Ref2.file[n].name, Block_Image_Group_Ref2.file[n].mapset);
	printf ("---------------------------\n");
	if (G_yes("Look ok? ",-1)) break;
	I_free_block_Block_Image_Group_Ref (&Block_Image_Group_Ref2);
    }
    if (I_put_subblock_Block_Image_Group_Ref (block, subblock, &Block_Image_Group_Ref2))
	printf ("Subblock [%s] created\n", subblock);
    else
	printf ("Subblock [%s] not created\n", subblock);
}





