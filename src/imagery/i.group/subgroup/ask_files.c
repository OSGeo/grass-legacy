#include <string.h>
#include "imagery.h"
#include "vask.h"

#define NFILES 10

int ask_files (char *group, char *subgroup, struct Ref *ref1, struct Ref *ref2)
{
    char use[NFILES][3];
    char name[NFILES][80];
    char *nm, *mp;
    int i,f,p,n;
    int row;
    char instructions[80];
    char next[20];
    char next_line[80];

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

	*next = 0;
	*next_line = 0;
	sprintf (next, "%s","___");
	sprintf (next_line, "%*s%*s ", 1, "Enter 'end' to end the add query mode: ",3,"");
	V_line (row+3, next_line);
	V_ques (next, 's', row+3, 39, 3);

	V_intrpt_ok();
	if(!V_call())
	    return 0;

	for (n=0; n < i; n++)
	    if (use[n][0])
		I_add_file_to_group_ref (ref1->file[p+n].name, ref1->file[p+n].mapset, ref2);

	if (strcmp (next, "end") == 0)
	  break;
	else
	   *next = 0;

    }
    return 1;
}
