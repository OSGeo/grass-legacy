#include "imagery.h"
main()
{
    char group[30];
    struct Ref ref;

    G_gisinit("");

    if (!I_ask_group_old("", group)) exit(0);
    I_get_group_ref (group, &ref);
    switch (I_read_group_colors (group, &ref))
    {
    case -1: printf ("ERROR\n"); break;
    case 0:  printf ("NO COLORS\n"); break;
    default:
	if (ref.red.n >= 0) printf ("RED: %s in %s\n",
		ref.file[ref.red.n].name, ref.file[ref.red.n].mapset);
	if (ref.grn.n >= 0) printf ("GRN: %s in %s\n",
		ref.file[ref.grn.n].name, ref.file[ref.grn.n].mapset);
	if (ref.blu.n >= 0) printf ("BLU: %s in %s\n",
		ref.file[ref.blu.n].name, ref.file[ref.blu.n].mapset);
	/*
	I_write_group_colors (group, &ref);
	*/
	break;
    }
}
