
#include "imagery.h"

static char *intro[]=
{
"This program targets an imagery group to a GRASS database",
0};

main(argc, argv) char *argv[];
{
    int i;
    char group[40], location[40], mapset[40];

    G_gisinit (argv[0]);

    for (i=0; intro[i]; i++)
	printf ("%s\n", intro[i]);

    if (!I_ask_group_old ("Enter group that needs a target",group))
	exit(0);
    I_get_target (group, location, mapset);
    G__create_alt_env();
    ask_target (group, location, mapset);
    G__switch_env();
    I_put_target (group, location, mapset);

    printf ("group [%s] targeted for location [%s], mapset [%s]\n",
	group, location, mapset);
    exit(0);
}
