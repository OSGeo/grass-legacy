
#include "imagery.h"
#include "local_proto.h"

int 
make_subgroup (char *group, char *subgroup, struct Ref *ref1)
{
    struct Ref ref2;
    int n;

    while (1)
    {
	I_init_group_ref (&ref2);
	if (!ask_files (group, subgroup, ref1, &ref2))
	{
	    fprintf (stderr, "Subgroup [%s] not created\n", subgroup);
	    exit(1);
	}
	if (ref2.nfiles <= 0)
	{
	    fprintf (stderr, "No files selected! Subgroup [%s] not created\n", subgroup);
	    exit(1);
	}
	fprintf (stderr, "Subgroup [%s] references the following raster file%s\n",
		subgroup, ref2.nfiles==1?"":"s");
	fprintf (stderr, "---------------------------\n");
	for (n = 0; n < ref2.nfiles; n++)
	    fprintf (stdout,"\t%s in %s\n", ref2.file[n].name, ref2.file[n].mapset);
	fprintf (stderr, "---------------------------\n");
	if (G_yes("Look ok? ",-1)) break;
	I_free_group_ref (&ref2);
    }
    if (I_put_subgroup_ref (group, subgroup, &ref2))
	fprintf (stderr, "Subgroup [%s] created\n", subgroup);
    else
	fprintf (stderr, "Subgroup [%s] not created\n", subgroup);

    return 0;
}
