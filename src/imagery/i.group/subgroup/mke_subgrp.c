
#include "imagery.h"

make_subgroup (group, subgroup, ref1)
    char *group;
    char *subgroup;
    struct Ref *ref1;
{
    struct Ref ref2;
    int n;

    while (1)
    {
	I_init_group_ref (&ref2);
	if (!ask_files (group, subgroup, ref1, &ref2))
	{
	    printf ("Subgroup [%s] not created\n", subgroup);
	    exit(1);
	}
	if (ref2.nfiles <= 0)
	{
	    printf ("No files selected! Subgroup [%s] not created\n", subgroup);
	    exit(1);
	}
	printf ("Subgroup [%s] references the following raster file%s\n",
		subgroup, ref2.nfiles==1?"":"s");
	printf ("---------------------------\n");
	for (n = 0; n < ref2.nfiles; n++)
	    printf ("\t%s in %s\n", ref2.file[n].name, ref2.file[n].mapset);
	printf ("---------------------------\n");
	if (G_yes("Look ok? ",-1)) break;
	I_free_group_ref (&ref2);
    }
    if (I_put_subgroup_ref (group, subgroup, &ref2))
	printf ("Subgroup [%s] created\n", subgroup);
    else
	printf ("Subgroup [%s] not created\n", subgroup);
}
