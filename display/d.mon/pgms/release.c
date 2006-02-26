/* release.mon - turn loose of a monitor */
/* if no name is specified, release current monitor; otherwise, release */
/* monitor specified */

#include <stdio.h>
#include <stdlib.h>
#include "gis.h"
#include "monitors.h"
#include "open.h"
#include "raster.h"
#include "local_proto.h"

int 
main (int argc, char *argv[])
{
    int silent, override;
    char *monitor;
    char *option;
    char *me;

    override = silent = 0;
    me = argv[0];
    while(argc > 1 && argv[1][0] == '-')
    {
	if (argv[1][1] == 0) usage (me);
	option = &argv[1][1];

	while (*option)
	{
	    switch (*option)
	    {
	    case 'f': override = 1; break;
	    case 'v': silent = 1; break;
	    default:  fprintf (stderr, "%s: -%c ** unrecognized option\n",
		    me, *option);
		      usage (me);
	    }
	    option++;
	}
	argv++;
	argc--;
    }
    if (argc > 2) usage (me);

    G_gisinit(argv[0]) ;
    if (argc > 1)
        G__setenv("MONITOR",argv[1]);
    monitor = G__getenv("MONITOR");
    if (!monitor) exit(1);

    if (override)
        R_release_driver();
    else
    {
        R__open_quiet();            /* call R_open_driver to see if we */
        switch (R_open_driver())        /*  own the monitor */
        {
        case OK:                /* if not locked or locked by us */
            R_close_driver();
        case NO_RUN:            /*   or not even in memory, */
            R_release_driver();     /*   we may release */
	    if (!silent)
		fprintf(stderr,"Monitor <%s> released\n", monitor);
            break;
        case LOCKED:            /* if locked by another, fail */
	    if (!silent)
		fprintf(stderr,"Note - Monitor <%s> in use by another user\n",
		     monitor);
            break;
        case NO_MON:            /* if no such monitor, fail */
	    if (!silent)
		fprintf(stderr,"Error - No such monitor as <%s>\n",monitor);
            break;
        default:                /* couldn't access lock file? */
	    if (!silent)
		fprintf(stderr,"Error - Failed testing lock mechanism\n");
            break;
        }
    }
    G_unsetenv ("MONITOR");

	return 0;
}

int 
usage (char *me)
{
    fprintf(stderr,"Usage:  %s [-fv] [name]\n",me);
    exit(1);
}
