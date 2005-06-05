/* 
 * Original INTER author: M. Shapiro
 * New cmd line version by Bob Covill 10/2001
 */
 
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "glocale.h"
#include "imagery.h"
#include "local_proto.h"


int 
main (int argc, char *argv[])
{
    char group[40], location[40], mapset[40];
    struct Option *grp, *map, *loc;
    struct GModule *module;
    struct Flag *c;

    G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
        	_("Targets an imagery group to a GRASS location and mapset.");
        	
	grp = G_define_option();
	grp->key             = "group";
	grp->type            =  TYPE_STRING;
	grp->required        =  YES;
	grp->gisprompt        = "old,group,group";
	grp->description     = _("Name of imagery group");

	loc = G_define_option();
	loc->key             = "location";
	loc->type            =  TYPE_STRING;
	loc->required        =  NO;
	loc->description     = _("Name of imagery target location");

	map = G_define_option();
	map->key             = "mapset";
	map->type            =  TYPE_STRING;
	map->required        =  NO;
	map->description     = _("Name of target mapset");

	c = G_define_flag();
	c->key              = 'c';
	c->description      = _("Set current location and mapset as target for of imagery group");

	if (G_parser (argc, argv))
		exit (-1);

	strcpy(group, grp->answer);

  /* my goodness: !! Isn't that easier? */
  if ( 
       ( (!c->answer) &&
         ( (!map->answer && loc->answer) || (map->answer && !loc->answer)  
            || (!map->answer && !loc->answer)
         )
       ) ||
       ( (c->answer) &&
         ( (!map->answer && loc->answer) || (map->answer && !loc->answer) 
            || (map->answer && loc->answer)
         )
       ) 
     ) {
	G_fatal_error(_("You must use either the Current Mapset and "
             "Location Flag (-c)\n OR\n manually enter the variables"));
   }

	if (c->answer) {
	  strcpy(mapset, G_mapset() );
	  strcpy(location, G_location() );
	} 
	else {
	  strcpy(mapset, map->answer);
	  strcpy(location, loc->answer);
	}

	I_put_target (group, location, mapset);

    G_message(_("group [%s] targeted for location [%s], mapset [%s]\n"),
	group, location, mapset);
    exit(0);
}
