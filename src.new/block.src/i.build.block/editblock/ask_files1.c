#include "dba_imagery.h"

#define NFILES 10

ask_oldfiles (Block_Image_Group_Ref1, Block_Image_Group_Ref2, block)
    struct Block_Image_Group_Ref *Block_Image_Group_Ref1, *Block_Image_Group_Ref2;
    char *block;
{
    char use[NFILES][3];
    char name[NFILES][80];
    int i,f,p,n,k;
    int repeat;
    int row;
    char title[80];
    char instructions[80];

    sprintf (instructions, "BLOCK: %s", block);
    I_location_info (title, instructions);
    sprintf (instructions, "If you wish to delete a file from block [%s]", block);

    f = 0;
    while(f < Block_Image_Group_Ref1->nfiles)
    {
	V_clear();
	V_line (0, title);
	V_line (2, instructions);
	V_line (3, "remove the 'x' from in front of the file name");
	for (i=0; i < NFILES; i++)
	    strcpy(use[i], "x");
	row = 6;
	p = f;
	for (i=0; i < NFILES && f < Block_Image_Group_Ref1->nfiles; i++, f++)
	{
	    sprintf (name[i], "%s in %s", Block_Image_Group_Ref1->file[f].name, Block_Image_Group_Ref1->file[f].mapset);
	    if (G_find_file ("group", Block_Image_Group_Ref1->file[f].name, Block_Image_Group_Ref1->file[f].mapset) == NULL)
	    {
		strcat(name[i], "  <-imagery group does not exist");
		use[i][0] = 0;
	    }
	    V_const (name[i], 's', row, 3, 70);
	    V_ques (use[i], 's', row, 0, 2);
	    row++;
	}
	if (i==0) break;
	if (f < Block_Image_Group_Ref1->nfiles)
	    V_line (row+2,"(more on next page)");

	V_intrpt_ok();
	if(!V_call())
	    return 0;

	for (n=0; n < i; n++)
	    if (use[n][0])
		I_transfer_block_Block_Image_Group_Ref_file (Block_Image_Group_Ref1, p+n, Block_Image_Group_Ref2);
    }
    return 1;
}




