/* stop_mon - stop a running monitor */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "raster.h"
#include "gis.h"
#include "monitors.h"
#include "open.h"
#include "local_proto.h"

static char *me;

int 
main (int argc, char *argv[])
{
    int forced;
    char *option;

    me = argv[0];
    forced = 0;
    while(argc > 1 && argv[1][0] == '-')
    {
	if (argv[1][1] == 0) usage (me);
	option = &argv[1][1];

	while (*option)
	{
	    switch (*option)
	    {
	    case 'f': forced = 1; break;
	    default:  fprintf (stderr, "%s: -%c ** unrecognized option\n",
		    me, *option);
		      usage (me);
	    }
	    option++;
	}
	argv++;
	argc--;
    }
    if (argc != 2)
	usage (me);
    stop_mon(argv[1], forced);

    return 0;
}
int 
usage (char *me)
{
    fprintf(stderr,"Usage: %s [-f] monitor_name\n", me);
    exit(1);
}

int 
stop_mon (char *name, int forced)
{
    char *cur;
    int unset;

    unset = 0;
    cur = G__getenv("MONITOR");
    if (cur != NULL && strcmp (cur, name) == 0)
	unset = 1;
    G__setenv("MONITOR",name);
    if (forced)
	R_release_driver();
    R__open_quiet();			/* call open_driver in quiet mode */
    switch (R_open_driver())
    {
    case OK:
	    R_kill_driver();
	    /*R_close_driver();*/
	    fprintf(stderr,"Monitor '%s' terminated\n",name);
	    break;
    case NO_RUN:
	    fprintf(stderr,"Error - Monitor '%s' was not running\n",name);
	    break;
    case NO_MON:
	    fprintf(stderr,"Error - No such monitor as '%s'\n",name);
	    break;
    case LOCKED:
	    fprintf(stderr,"Error - Monitor '%s' in use by another user\n",name);
	    break;
    default:
	    fprintf(stderr,"Error - Locking mechanism failed\n");
	    break;
    }
    if (unset)
	G_unsetenv ("MONITOR");

    return 0;
}

