/* read the target for the block and cast it into the alternate GRASS env */

#include <stdio.h>
#include "dba_imagery.h"
#include "elev.h"

static int which_env;
static int stat = 0;


main(argc, argv) char *argv[];
{
    int i;
    char *block, *location, *mapset, buf[100];

    location    = (char *) G_malloc (80*sizeof (char));
    mapset      = (char *) G_malloc (80*sizeof (char));
    elev_layer  = (char *) G_malloc (80*sizeof (char));
    mapset_elev = (char *) G_malloc (80*sizeof (char));
    tl          = (char *) G_malloc (80*sizeof (char));
    math_exp    = (char *) G_malloc (80*sizeof (char));
    units       = (char *) G_malloc (80*sizeof (char));
    nd          = (char *) G_malloc (80*sizeof (char));
    block       = (char *) G_malloc (80*sizeof (char));

    if (argc != 2)
    {
	fprintf (stderr, "Usage: %s block\n", argv[0]);
	exit(1);
    }

    G_gisinit (argv[0]);
    block = argv[1];

    G_suppress_warnings(1);
    if (!I_get_block_target(block, location, mapset))
    {
	sprintf(buf, "Target information for block [%s] missing\n", block);
	goto error;
    }

    G_suppress_warnings(0);
    sprintf (buf, "%s/%s", G_gisdbase(), location);
    if (access(buf,0) != 0)
    {
	sprintf (buf,"Target location [%s] not found\n", location);
	goto error;
    }
    I_get_block_elev (block, elev_layer, mapset_elev, tl, math_exp, units, nd);
    G__create_alt_env();
    G__setenv ("LOCATION_NAME", location);
    stat = G__mapset_permissions(mapset);
    if (stat > 0)
    {
	G__setenv ("MAPSET", mapset);
	G__create_alt_search_path();
	G__switch_env();
	G__switch_search_path();
	which_env = 0;

           /* get elevation layer cell file  in target location */
       select_target_env(); 
       ask_elev(block,location,mapset);

	   /* select current location */
       select_current_env(); 
       I_put_block_elev (block,elev_layer,mapset_elev,tl, math_exp, units, nd);
       return 1;
    }
    sprintf (buf, "Mapset [%s] in target location [%s] - ",
		mapset, location);
    strcat (buf, stat == 0 ? "permission denied\n" : "not found\n");

error:
    strcat (buf, "Please select a target for block ");
    strcat (buf, block);
    G_suppress_warnings(0);
    G_fatal_error (buf);
}

select_current_env()
{
    if (which_env != 0)
    {
	G__switch_env();
	G__switch_search_path();
	which_env = 0;
    }
}

select_target_env()
{
    if (which_env != 1)
    {
	G__switch_env();
	G__switch_search_path();
	which_env = 1;
    }
}




