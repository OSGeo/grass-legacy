#include "dba_imagery.h"

#define NFILES 10

ask_one_file (blockname, subblock, groupname, block_ref)
    char *blockname, *subblock, *groupname;
    struct Block_Image_Group_Ref block_ref;
{
    char use[NFILES][3];
    char name[NFILES][80];
    char msg[80];
    int i,f,p,n,k,count;
    int repeat;
    int row;
    char title[80];
    char instructions[80];

    sprintf (instructions, "BLOCK: %s  SUB_BLK %s", blockname, subblock);
    I_location_info (title, instructions);
    sprintf (instructions, "Select one imagery group to initialize camera\n exposere station parameters\n");

    f = 0;
    while(f < block_ref.nfiles)
    {
	V_clear();
	V_line (0, title);
	V_line (2, instructions);
	V_line (3, "place an 'x' in front of the imagery group name");
	for (i=0; i < NFILES; i++)
	    strcpy(use[i], "");
	row = 6;
	p = f;
	for (i=0; i < NFILES && f < block_ref.nfiles; i++, f++)
	{
	    sprintf (name[i], "%s in %s", block_ref.file[f].name, block_ref.file[f].mapset);
	    if (G_find_file ("group", block_ref.file[f].name, block_ref.file[f].mapset) == NULL)
	    {
		strcat(name[i], "  <-imagery group does not exist");
		use[i][0] = 0;
	    }
	    V_const (name[i], 's', row, 3, 70);
	    V_ques (use[i], 's', row, 0, 2);
	    row++;
	}
	if (i==0) break;
	if (f < block_ref.nfiles)
	    V_line (row+2,"(more on next page)");

	V_intrpt_ok();
	if(!V_call())
	    return 0;

	if (i==0) break;
	count = 0;
	for (n=0; n < i; n++)
	    if (use[n][0])
	      { count++;
        	strcpy (groupname, block_ref.file[n].name);
	      }
        if (count > 1)
	  {  sprintf (msg,"You may select just one imagery group\n");
	     G_warning (msg);
             continue;
	  }
    }
    if (count == 1)
      return 1;
    return 0;
}




