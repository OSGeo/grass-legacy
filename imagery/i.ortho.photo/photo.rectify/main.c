
/****************************************************************************
 *
 * MODULE:       photo.rectify
 * AUTHOR(S):    Mike Baba,  DBA Systems, Inc. (original contributor)
 *               Markus Neteler <neteler itc.it>, 
 *               Bernhard Reiter <bernhard intevation.de>, 
 *               Glynn Clements <glynn gclements.plus.com>, 
 *               Hamish Bowman <hamish_b yahoo.com>
 *
 * PURPOSE:      Rectifies an image by using the image to photo coordinate 
 *                 transformation matrix
 * COPYRIGHT:    (C) 1999-2008 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/
#define GLOBAL
#include <stdlib.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/imagery.h>
#include <grass/glocale.h>
#include "global.h"

int main(int argc, char *argv[])
{

    char name[GNAME_MAX];
    char *camera;
    int n, nfiles;
    char tl[100];
    char math_exp[100];
    char units[100];
    char nd[100];

    struct GModule *module;
    struct Option *group_opt;


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


    G_suppress_masking();	/* need to do this for target location */

    strcpy(name, group_opt->answer);

    camera = (char *)G_malloc(40 * sizeof(char));
    elev_layer = (char *)G_malloc(GNAME_MAX * sizeof(char));
    mapset_elev = (char *)G_malloc(GMAPSET_MAX * sizeof(char));

    /* get group ref */
    strcpy(group.name, name);
    if (!I_find_group(group.name))
	G_fatal_error(_("Group [%s] not found"), group.name);

    /* get the group ref */
    I_get_group_ref(group.name, (struct Ref *)&group.group_ref);
    nfiles = group.group_ref.nfiles;
    if (nfiles <= 0)
	G_fatal_error(_("No files in this group!"));

    ref_list = (int *)G_malloc(nfiles * sizeof(int));
    new_name = (char **)G_malloc(nfiles * sizeof(char *));
    for (n = 0; n < nfiles; n++)
	ref_list[n] = -1;

    /* get the target */
    get_target(group.name);

    /* ask for files to be rectified */
    ask_files(group.name);


    G_debug(1, "Looking for elevation file in group: <%s>", group.name);

    /* get the block elevation layer raster map  in target location */
    if (!I_get_group_elev(group.name, elev_layer, mapset_elev, tl,
			 math_exp, units, nd))
	G_fatal_error(_("No target elevation model selected for group <%s>"),
		      group.name);

    G_debug(1, "Block elevation: <%s> in <%s>", elev_layer, mapset_elev);

    /* get the elevation layer header in target location */
    select_target_env();
    G_get_cellhd(elev_layer, mapset_elev, &elevhd);
    select_current_env();

    /** look for camera info  for this block **/
    if (!I_get_group_camera(group.name, camera))
	G_fatal_error(_("No camera reference file selected for group <%s>"),
		      group.name);

    if (!I_get_cam_info(camera, &group.camera_ref))
	G_fatal_error(_("Bad format in camera file for group <%s>"),
		      group.name);

    /* get initial camera exposure station, if any */
    if (I_find_initial(group.name)) {
	if (!I_get_init_info(group.name, &group.camera_exp))
	    G_warning(_("Bad format in initial exposusre station file for group <%s>"),
		      group.name);
    }

    /* read the reference points for the group, compute image-to-photo trans. */
    get_ref_points();

    /* read the control points for the group, convert to photo coords. */
    get_conz_points();

    /* ask for window to be used in target location */
    select_current_env();
    get_target_window();

    /* modify elevation values if needed */

    /***
    select_target_env();
    ask_elev_data();
    select_current_env();
    ***/

    /*  go do it */
    exec_rectify();

    exit(EXIT_SUCCESS);
}
