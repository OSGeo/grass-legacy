
#include <stdio.h>
#include "dba_imagery.h"

main(argc, argv) char *argv[];
{
    int i;
    char *block, *location, *mapset;

    location = (char *) G_malloc (80*sizeof (char));
    mapset   = (char *) G_malloc (80*sizeof (char));
    block    = (char *) G_malloc (80*sizeof (char));

    if (argc != 2)
    {
	fprintf (stderr, "Usage: %s block\n", argv[0]);
	exit(1);
    }

    G_gisinit (argv[0]);
    block = argv[1];

    I_get_block_target (block, location, mapset);
    G__create_alt_env();
    ask_target (block, location, mapset);
    G__switch_env();
    I_put_block_target (block, location, mapset);

    printf ("block [%s] targeted for location [%s], mapset [%s]\n",
	block, location, mapset);
    exit(0);
}


