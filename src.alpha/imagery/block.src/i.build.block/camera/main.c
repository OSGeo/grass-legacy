/* select a camera reference file for a given imagery block */

/* Mike Baba */
/* DBA Systems */
/* Fairfax VA. */ 

#include <stdio.h>
#include "camera.h"

main(argc, argv) char *argv[];
{
    static int i;
    char *block, *location, *mapset, *camera; 

    location = (char *) G_malloc (80*sizeof (char));
    mapset   = (char *) G_malloc (80*sizeof (char));
    block    = (char *) G_malloc (80*sizeof (char));
    camera   = (char *) G_malloc (80*sizeof (char));

    G_gisinit (argv[0]);
    location = (char *) G_location();
    mapset   = (char *) G_mapset();

    if (argc != 2)
    {
	fprintf (stderr, "Usage: %s block\n", argv[0]);
	exit(1);
    }

     block = argv[1];

     ask_camera (block,location,mapset);
   
     /* printf ("block [%s] in location [%s] mapset [%s] now has camera file [%s]\n", block, location, mapset, camera);*/
    exit(0);
}

