/*=======================================================================
				i.rectify
  target.c --

     int get_target (void)
          Read the target infomation for the imagery group.  Check that 
	  the target location and mapset are accessable.  If they are
	  create an alternative (secondary enviroment) so that we can
	  easily switch between both the source and target locations.
	  RETURNS     1 if target info is found and accessable,
	  OTHERWISE   calls G_fatal_error();

	  -- also read target_window.

     select_current_env(void)
          Set the internal GRASS enviroment to the current (source)
	  location.

     select_target_env(void)
          Set the internal GRASS enviroment to the target location.

     GrassEnv new_environment( GrassEnv newenv)
          GrassEnv is a typedef { CurrentEnv, TargetEnv }
	  new_enviroment will switch to the newenv by calling either
	  select_current_env() or select_target_env().  If we are 
	  currently in the enviroment which we request to switch to
	  new_enviroment will simply return without attempting the switch

=======================================================================*/

#include <string.h>
#include <unistd.h>
#include "global.h"


/*---------------------------------------------------------------------*/

int 
get_target (char *name)
{
    char location[40];
    char mapset[40];
    char buf[1024];
    int stat;

    if (!I_get_target(name, location, mapset))
    {
	sprintf (buf, "Target information for group [%s] missing\n", name);
	goto error;
    }

    sprintf (buf, "%s/%s", G_gisdbase(), location);
    if (access(buf,0) != 0)
    {
	sprintf (buf,"Target location [%s] not found\n", location);
	goto error;
    }
    select_target_env();
    G__setenv ("LOCATION_NAME", location);
    stat = G__mapset_permissions(mapset);
    if (stat > 0)
    {
	G__setenv ("MAPSET", mapset);
	G_get_window (&target_window);
	select_current_env();
	return 1;
    }
    sprintf (buf, "Mapset [%s] in target location [%s] - ",
		mapset, location);
    strcat (buf, stat == 0 ? "permission denied\n" : "not found\n");
error:
    strcat (buf, "Please run i.target for group ");
    strcat (buf, name);
    G_fatal_error (buf);
}


