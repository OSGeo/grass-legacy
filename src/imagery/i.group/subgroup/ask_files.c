#include "imagery.h"

#define NFILES 10

ask_files (group, subgroup, ref1, ref2)
    char *group;
    char *subgroup;
    struct Ref *ref1, *ref2;
{
    char use[NFILES][3];
    char name[NFILES][80];
    char *nm, *mp;
    int i,f,p,n;
    int repeat;
    int row;
    char instructions[80];

    sprintf (instructions, "Mark an 'x' by the files to form subgroup [%s]", subgroup);

    f = 0;
    while(f < ref1->nfiles)
    {
	V_clear();
	V_line (0, instructions);
	for (i=0; i < NFILES; i++)
	    use[i][0] = 0;
	row = 3;
	p = f;
	for (i=0; i < NFILES && f < ref1->nfiles; i++, f++)
	{
	    nm = ref1->file[f].name;
	    mp = ref1->file[f].mapset;
	    sprintf (name[i], "%s in %s", nm, mp);
	    V_const (name[i], 's', row, 3, 70);
	    if (G_find_cell (nm, mp) == NULL)
		strcat(name[i], "  <- raster file does not exist");
	    else
		V_ques (use[i], 's', row, 0, 2);
	    row++;
	}
	if (i==0) break;
	if (f < ref1->nfiles)
	    V_line (1, "(more on next page)");

	V_intrpt_ok();
	if(!V_call())
	    return 0;

	for (n=0; n < i; n++)
	    if (use[n][0])
		I_add_file_to_group_ref (ref1->file[p+n].name, ref1->file[p+n].mapset, ref2);
    }
    return 1;
}
