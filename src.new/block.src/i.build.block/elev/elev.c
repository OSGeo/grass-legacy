/* read the target for the block and cast it into the alternate GRASS env */

#include <stdio.h>
#include "dba_imagery.h"

static int which_env;
static int stat = 0;

main(argc, argv) char *argv[];
{
    int i;
    char *block, *elev_layer, *location, *mapset, *mapset_elev, buf[100];

    location    = (char *) G_malloc (80*sizeof (char));
    mapset      = (char *) G_malloc (80*sizeof (char));
    elev_layer  = (char *) G_malloc (80*sizeof (char));
    mapset_elev = (char *) G_malloc (80*sizeof (char));
    block       = (char *) G_malloc (80*sizeof (char));

    if (argc != 2)
    {
	fprintf (stderr, "Usage: %s block\n", argv[0]);
	exit(1);
    }

    G_gisinit (argv[0]);
    block = argv[1];

    if (!I_get_block_target(block, location, mapset))
    {
	sprintf(buf, "Target information for block [%s] missing\n", block);
	goto error;
    }

    sprintf (buf, "%s/%s", G_gisdbase(), location);
    if (access(buf,0) != 0)
    {
	sprintf (buf,"Target location [%s] not found\n", location);
	goto error;
    }
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
       if (!G_ask_cell_old ("Enter the name of an existing cell file in the \ntarget location to be used for elevation data", elev_layer)) exit(0);

           /* need the mapset where found */
       if ((mapset_elev = G_find_cell(elev_layer,"")) == NULL) exit(0);
	   /* select current location */
       select_current_env(); 
       return 1;
      }
    sprintf (buf, "Mapset [%s] in target location [%s] - ",
		mapset, location);
    strcat (buf, stat == 0 ? "permission denied\n" : "not found\n");
error:
    strcat (buf, "Please select a target for block ");
    strcat (buf, block);
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




