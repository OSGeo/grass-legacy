/*=======================================================================
				i.points
  target.c --

     int get_target (void)
          Read the target infomation for the imagery group.  Check that 
	  the target location and mapset are accessable.  If they are
	  create an alternative (secondary enviroment) so that we can
	  easily switch between both the source and target locations.
	  RETURNS     1 if target info is found and accessable,
	  OTHERWISE   calls G_fatal_error();

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
#include "globals.h"

         /* internal function prototypes */
#ifdef _NO_PROTO
#else
#endif

/*---------------------------------------------------------------------*/

/* which_env:
 *    0 == CurrentEnv == current environment
 *    1 == TargetEnv  == target environment
 */
static GrassEnv which_env;

/*---------------------------------------------------------------------*/
int get_target (void)
{
    char location[40];
    char mapset[40];
    char buf[1024];
    int  stat;

       /* get the target info for the group */
    if (!I_get_target(group.name, location, mapset))
    {
	sprintf(buf, "Target information for group [%s] missing\n", group.name);
	goto error;
    }

       /* check that the access to the target location/mapset is ok */
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
	which_env = CurrentEnv;
	return 1;
    }

    sprintf (buf, "Mapset [%s] in target location [%s] - ",
	     mapset, location);
    strcat (buf, stat == 0 ? "permission denied\n" : "not found\n");

error:
    strcat (buf, "Please run i.target for group ");
    strcat (buf, group.name);
    G_fatal_error (buf);
}

/*---------------------------------------------------------------------*/
int select_current_env(void)
{
    if (which_env != CurrentEnv)
    {
	G__switch_env();
	G__switch_search_path();
	which_env = CurrentEnv;
    }

    return 0;
}

/*---------------------------------------------------------------------*/
int select_target_env(void)
{
    if (which_env != TargetEnv)
    {
	G__switch_env();
	G__switch_search_path();
	which_env = TargetEnv;
    }

    return 0;
}

/*---------------------------------------------------------------------*/
GrassEnv new_environment(GrassEnv newenv)
{
   if( newenv == which_env)
      return(which_env);
   if(newenv == TargetEnv) {
      select_target_env();
      return(CurrentEnv);
   } else {
      select_current_env();
      return(TargetEnv);
   }

    return 0;
}
