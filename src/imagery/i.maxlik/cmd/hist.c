#include "gis.h"
make_history(name, group, subgroup, sigfile)
    char *name, *group, *subgroup, *sigfile;
{
    struct History hist;

    if(G_read_history (name, G_mapset(), &hist) >= 0)
    {
	sprintf (hist.datsrc_1, "Group/subgroup: %s/%s", group, subgroup);
	sprintf (hist.datsrc_2, "Signature file: %s", sigfile);
	G_write_history (name, &hist);
    }
}
