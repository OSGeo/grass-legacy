/* main.c */
#include <stdio.h>
#include "orthophoto.h"

main(argc, argv) char *argv[];
{
    char *group, *location, *mapset;

    location = (char *) G_malloc (80*sizeof (char));
    mapset   = (char *) G_malloc (80*sizeof (char));
    group    = (char *) G_malloc (80*sizeof (char));

    if (argc != 2)
    {
	fprintf (stderr, "Usage: %s group\n", argv[0]);
	exit(1);
    }

    G_gisinit (argv[0]);
    group = argv[1];

    I_get_target (group, location, mapset);
    G__create_alt_env();
    ask_target (group, location, mapset);
    G__switch_env();
    I_put_target (group, location, mapset);

    printf ("Group [%s] targeted for location [%s], mapset [%s]\n",
	group, location, mapset);
    exit(0);
}


