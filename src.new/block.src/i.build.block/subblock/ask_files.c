#include "dba_imagery.h"

#define NFILES 10

ask_files (block, subblock, Block_Image_Group_Ref1, Block_Image_Group_Ref2)
    char *block;
    char *subblock;
    struct Block_Image_Group_Ref *Block_Image_Group_Ref1, *Block_Image_Group_Ref2;
{
    char use[NFILES][3];
    char name[NFILES][80];
    char *nm, *mp;
    int i,f,p,n;
    int repeat;
    int row;
    char instructions[80];

    sprintf (instructions, "Mark an 'x' by the imagery groups to form subblock [%s]", subblock);

    f = 0;
    while(f < Block_Image_Group_Ref1->nfiles)
    {
	V_clear();
	V_line (0, instructions);
	for (i=0; i < NFILES; i++)
	    use[i][0] = 0;
	row = 3;
	p = f;
	for (i=0; i < NFILES && f < Block_Image_Group_Ref1->nfiles; i++, f++)
	{
	    nm = Block_Image_Group_Ref1->file[f].name;
	    mp = Block_Image_Group_Ref1->file[f].mapset;
	    sprintf (name[i], "%s in %s", nm, mp);
	    V_const (name[i], 's', row, 3, 70);
	    if (G_find_file ("group", nm, mp) == NULL)
		strcat(name[i], "  <- imagery group does not exist");
	    else
		V_ques (use[i], 's', row, 0, 2);
	    row++;
	}
	if (i==0) break;
	if (f < Block_Image_Group_Ref1->nfiles)
	    V_line (1, "(more on next page)");

	V_intrpt_ok();
	if(!V_call())
	    return 0;

	for (n=0; n < i; n++)
	    if (use[n][0])
		I_add_file_to_block_Block_Image_Group_Ref (Block_Image_Group_Ref1->file[p+n].name, Block_Image_Group_Ref1->file[p+n].mapset, Block_Image_Group_Ref2);
    }
    return 1;
}


