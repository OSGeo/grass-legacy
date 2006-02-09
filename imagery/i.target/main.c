/* 
 * Original INTER author: M. Shapiro
 * New cmd line version by Bob Covill 10/2001
 * Rewritten for GRASS6 by Brad Douglas 08/2005
 */
 
#include <stdlib.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/glocale.h>
#include <grass/imagery.h>


int 
main (int argc, char *argv[])
{
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

    if (G_parser(argc, argv))
        exit(EXIT_FAILURE);

    /* error if -c is specified with other options */
    if ((!c->answer) && (!map->answer || !loc->answer))
	G_fatal_error(_("You must use either the Current Mapset and "
             "Location Flag (-c)\n OR\n manually enter the variables."));

    /* error if -c is not specified and not enough other options */
    if ((c->answer) && (map->answer || loc->answer))
	G_fatal_error(_("You must use either the Current Mapset and "
             "Location Flag (-c)\n OR\n manually enter the variables."));

    if (c->answer) {
        /* point group target to current mapset and location */
        I_put_target(grp->answer, G_location(), G_mapset());
        G_message(_("group [%s] targeted for location [%s], mapset [%s]\n"),
                grp->answer, G_location(), G_mapset());
    } else {
        /* point group target to specified mapset and location */
        I_put_target(grp->answer, loc->answer, map->answer);
        G_message(_("group [%s] targeted for location [%s], mapset [%s]\n"),
                grp->answer, loc->answer, map->answer);
    }

    return 0;
}
