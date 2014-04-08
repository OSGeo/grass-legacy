
/****************************************************************************
 *
 * MODULE:       i.photo.init
 * AUTHOR(S):    Mike Baba,  DBA Systems, Inc. (original contributor)
 *               Markus Neteler <neteler itc.it>, 
 *               Roberto Flor <flor itc.it>, 
 *               Bernhard Reiter <bernhard intevation.de>, 
 *               Glynn Clements <glynn gclements.plus.com>
 *               Hamish Bowman
 *
 * PURPOSE:      Creates or modifies entries in a camera initial exposure
 *               station file for imagery group referenced by a sub-block
 * COPYRIGHT:    (C) 1999-2008 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/

#define  MAIN  1
#include <stdlib.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/glocale.h>
#include "globals.h"


int main(int argc, char *argv[])
{
    struct GModule *module;
    struct Option *group_opt;

    char name[GNAME_MAX];
    int have_old;

    /* must run in a term window */
    G_putenv("GRASS_UI_TERM", "1");

    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("imagery, orthorectify");
    module->description =
	_("Interactively creates or modifies entries in a camera "
	  "initial exposure station file for imagery group referenced "
	  "by a sub-block.");

    group_opt = G_define_standard_option(G_OPT_I_GROUP);
    group_opt->description =
	_("Name of imagery group for ortho-rectification");

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);


    strcpy(name, group_opt->answer);

    /* get group ref */
    strcpy(group.name, name);

    if (!I_find_group(group.name)) {
	G_fatal_error(_("Group [%s] not found"), name);
    }
    G_debug(1, "Found group %s", group.name);


/*******************
    I_get_Ortho_Image_Group_Ref(group.name, &group.group_ref);
    nfiles = block.block_ref.nfiles;
    G_debug(1, "Got group ref");
*******************/


    /* get initial camera exposure info */
    if (I_find_initial(group.name)) {
	have_old = 1;
	I_get_init_info(group.name, &group.camera_exp);
    }

    /* modify info */
    mod_init_info(have_old, &group.camera_exp);

    /* save info */
    I_put_init_info(group.name, &group.camera_exp);

    exit(EXIT_SUCCESS);
}
