
/****************************************************************************
 *
 * MODULE:       photo.elev
 * AUTHOR(S):    Mike Baba (original contributor)
 *               Markus Neteler <neteler itc.it>, 
 *               Roberto Flor <flor itc.it>, 
 *               Bernhard Reiter <bernhard intevation.de>, 
 *               Glynn Clements <glynn gclements.plus.com>
 *               Hamish Bowman
 *
 * PURPOSE:      Select the elevation model
 * COPYRIGHT:    (C) 1999-2008 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/

/* read the target for the block and cast it into the alternate GRASS env */
#define MAIN
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/imagery.h>
#include <grass/glocale.h>
#include "orthophoto.h"
#include "elev.h"

static int which_env;

char *elev_layer;
char *mapset_elev;
char *tl;
char *math_exp;
char *units;
char *nd;

int main(int argc, char *argv[])
{

    struct GModule *module;
    struct Option *group_opt;

    char location[GMAPSET_MAX];
    char mapset[GMAPSET_MAX];
    char group[GNAME_MAX];

    char buf[100];
    int stat;


    /* must run in a term window */
    G_putenv("GRASS_UI_TERM", "1");

    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("imagery, orthorectify");
    module->description =
	_("Interactively select or modify the target elevation model.");

    group_opt = G_define_standard_option(G_OPT_I_GROUP);
    group_opt->description =
	_("Name of imagery group for ortho-rectification");

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    elev_layer = (char *)G_malloc(GNAME_MAX * sizeof(char));
    mapset_elev = (char *)G_malloc(GMAPSET_MAX * sizeof(char));
    tl = (char *)G_malloc(80 * sizeof(char));
    math_exp = (char *)G_malloc(80 * sizeof(char));
    units = (char *)G_malloc(80 * sizeof(char));
    nd = (char *)G_malloc(80 * sizeof(char));

    *elev_layer = 0;
    *mapset_elev = 0;
    *tl = 0;
    *math_exp = 0;
    *units = 0;
    *nd = 0;

    strcpy(group, group_opt->answer);

    G_suppress_warnings(1);
    if (!I_get_target(group, location, mapset)) {
	sprintf(buf, _("Target information for group [%s] missing\n"), group);
	goto error;
    }

    G_suppress_warnings(0);
    sprintf(buf, "%s/%s", G_gisdbase(), location);
    if (access(buf, 0) != 0) {
	sprintf(buf, _("Target location [%s] not found\n"), location);
	goto error;
    }

    I_get_group_elev(group, elev_layer, mapset_elev, tl, math_exp, units, nd);
    G__create_alt_env();
    G__setenv("LOCATION_NAME", location);

    stat = G__mapset_permissions(mapset);
    if (stat > 0) {
	G__setenv("MAPSET", mapset);
	G__create_alt_search_path();
	G__switch_env();
	G__switch_search_path();
	which_env = 0;

	/* get elevation layer raster map  in target location */
	select_target_env();
	ask_elev(group, location, mapset);

	/* select current location */
	select_current_env();

	I_put_group_elev(group, elev_layer, mapset_elev, tl,
			 math_exp, units, nd);

	exit(EXIT_SUCCESS);
    }

    sprintf(buf, _("Mapset [%s] in target location [%s] - "), mapset, location);
    strcat(buf, stat == 0 ? _("permission denied\n") : _("not found\n"));

  error:
    strcat(buf, _("Please select a target for group"));
    strcat(buf, group);
    G_suppress_warnings(0);
    G_fatal_error(buf);
}


int select_current_env(void)
{
    if (which_env != 0) {
	G__switch_env();
	G__switch_search_path();
	which_env = 0;
    }

    return 0;
}

int select_target_env(void)
{
    if (which_env != 1) {
	G__switch_env();
	G__switch_search_path();
	which_env = 1;
    }

    return 0;
}
