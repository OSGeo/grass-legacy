
#include "imagery.h"

static char title[80];
static char *info[]=
{
title,
"",
"Please enter the subgroup to be created",
NULL
};

main(argc,argv) char *argv[];
{
    char *group;
    char subgroup[30];
    struct Ref ref;

    if (argc != 2)
    {
	fprintf (stderr, "usage: %s group\n", argv[0]);
	exit(1);
    }

    G_gisinit (argv[0]);
    group = argv[1];
    if (!I_find_group (group))
    {
	fprintf (stderr, "** Group [%s] not found\n", group);
	exit(1);
    }

    I_get_group_ref (group, &ref);
    if (ref.nfiles <= 0)
    {
	fprintf (stderr, "** Group [%s] contains no files. Can't form any subgroups\n", group);
	exit(1);
    }
    I_location_info (title, "");
    if (I_vask_subgroup_new (info, group, subgroup, 0, ""))
	make_subgroup (group, subgroup, &ref);
    exit(0);
}
