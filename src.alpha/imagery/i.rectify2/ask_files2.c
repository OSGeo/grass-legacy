#include "global.h"

#define NFILES 15

ask_file_from_list (name, mapset)
    char *name, *mapset;
{
    char line[NFILES][75];
    char use[NFILES][3];
    int index[NFILES];
    int i,k,n,f1,f2,ln;
    int count;
    int same;
    struct Cell_head win1, win2;

    count = 0;
    same = 1;
    for (f1=0; f1 < ref.nfiles; f1++)
    {	
	if (ref_list[f1] >= 0)
	    if (count++ == 0)
	    {
		f2 = ref_list[f1];
		G_get_cellhd (ref.file[f2].name, ref.file[f2]. mapset, &win1);
	    }
	    else if (same)
	    {
		k = ref_list[f1];
		G_get_cellhd (ref.file[k].name, ref.file[k]. mapset, &win2);
		if (win1.north  != win2.north ||
		    win1.south  != win2.south ||
		    win1.east   != win2.east  ||
		    win1.west   != win2.west  ||
		    win1.ns_res != win2.ns_res  ||
		    win1.ew_res != win2.ew_res)
			same = 0;
	    }

    }
    if (count == 1 || same)
    {
	strcpy (name, ref.file[f2].name);
	strcpy (mapset, ref.file[f2].mapset);
	return;
    }
    while(1)
    {
	f1 = 0;
	for (f2 = f1; f1 < ref.nfiles; f1 = f2)
	{
	    ln = 3;
	    V_clear();
	    V_line (0, "Please mark one file to use as a reference for the region");
	    for (i=0; i < NFILES; i++)
		use[i][0] = 0;
	    for (i=0; f2 < ref.nfiles && i < NFILES; i++, f2++)
	    {
		if (ref_list[f2] < 0)
		{
		    i--;
		    continue;
		}
		n = index[i] = ref_list[f2];
		sprintf (line[i], "   %s@%s", ref.file[n].name, ref.file[n].mapset);
		V_line (ln, line[i]);
		V_ques (use[i], 's', ln, 1, 1);
		ln++;
	    }
	    if (i==0) break;
	    V_intrpt_ok();
	    if(!V_call())
		exit(0);

	    for (i=0; i < NFILES; i++)
	    {
		if (use[i][0])
		{
		    n = index[i];
		    strcpy (name, ref.file[n].name);
		    strcpy (mapset, ref.file[n].mapset);
		    return;
		}
	    }
	}
    }
}
