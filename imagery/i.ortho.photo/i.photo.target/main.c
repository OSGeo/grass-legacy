
/****************************************************************************
 *
 * MODULE:       i.photo.target
 * AUTHOR(S):    Mike Baba,  DBA Systems, Inc. (original contributor)
 *               Markus Neteler <neteler itc.it>,
 *               Roberto Flor <flor itc.it>, 
 *               Bernhard Reiter <bernhard intevation.de>, 
 *               Glynn Clements <glynn gclements.plus.com>
 *               Hamish Bowman
 *
 * PURPOSE:      Select target location and mapset
 * COPYRIGHT:    (C) 1999-2008 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/imagery.h>
#include <grass/glocale.h>
#include "orthophoto.h"
#include "local_proto.h"

int main(int argc, char *argv[])
{
    struct GModule *module;
    struct Option *group_opt;

    char location[GMAPSET_MAX];
    char mapset[GMAPSET_MAX];
    char group[GNAME_MAX];


    /* must run in a term window */
    G_putenv("GRASS_UI_TERM", "1");

    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("imagery, orthorectify");
    module->description =
	_("Interactively select or modify the imagery group target.");

    group_opt = G_define_standard_option(G_OPT_I_GROUP);
    group_opt->description =
	_("Name of imagery group for ortho-rectification");

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);


    strcpy(group, group_opt->answer);

    I_get_target(group, location, mapset);
    G__create_alt_env();
    ask_target(group, location, mapset);
    G__switch_env();
    I_put_target(group, location, mapset);

    G_message(_("Group [%s] targeted for location [%s], mapset [%s]"),
	    group, location, mapset);

    exit(EXIT_SUCCESS);
}
