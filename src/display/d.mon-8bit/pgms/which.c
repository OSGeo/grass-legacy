/* which_mon - show name of currently selected monitor */

#include <stdio.h>
#include "gis.h"

main(argc,argv) char *argv[];
{
    char *name;

    G_gisinit (argv[0]);
    if ((name = G__getenv("MONITOR")) == NULL)
	printf("No monitor currently selected for output\n");
    else
	printf("Currently selected monitor: %s\n",name);
    exit(0);
}
