
/****************************************************************************
 *
 * MODULE:       photo.camera
 * AUTHOR(S):    Mike Baba,  DBA Systems, Inc. (original contributor)
 *               Markus Neteler <neteler itc.it>,
 *               Roberto Flor <flor itc.it>, 
 *               Bernhard Reiter <bernhard intevation.de>, 
 *               Glynn Clements <glynn gclements.plus.com>
 *               Hamish Bowman
 *
 * PURPOSE:      Creates or modifies entries in a camera reference file
 * COPYRIGHT:    (C) 1999-2008 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/
/* select_camera */
/* select a camera reference file for a given imagery group */

#define  MAIN   1
#include <stdlib.h>
#include <string.h>

#ifdef DEBUG
#include <unistd.h> /* for sleep() */
#endif

#include <grass/gis.h>
#include <grass/glocale.h>
#include "orthophoto.h"
#include "globals.h"
int main(int argc, char *argv[])
{
    struct GModule *module;
    struct Option *group_opt, *camera_opt;

    char *location;
    char *mapset;
    char group[GNAME_MAX];

    static int have_old;
    char *camera;


    /* must run in a term window */
    G_putenv("GRASS_UI_TERM", "1");

    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("imagery, orthorectify");
    module->description =
	_("Interactively select and modify the imagery group camera reference file.");

    group_opt = G_define_standard_option(G_OPT_I_GROUP);
    group_opt->description =
	_("Name of imagery group for ortho-rectification");

    camera_opt = G_define_standard_option(G_OPT_F_INPUT);
    camera_opt->key = "camera";
    camera_opt->required = NO;
    camera_opt->gisprompt = "old_file,camera,camera";
    camera_opt->description =
	_("Name of camera reference file to use");

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);


    camera = (char *)G_malloc(GNAME_MAX * sizeof(char));

    location = G_location();
    mapset = G_mapset();

    strcpy(group, group_opt->answer);

    if( !camera_opt->answer ) {
	/* select the camera to use */
	if (!I_ask_camera_any(
	    _("Enter a camera reference file to be used with this imagery group"),
	      camera)) {
	    exit(EXIT_SUCCESS);
	}
    }
    else {
	if (G_legal_filename (camera_opt->answer) < 0)
	    G_fatal_error(_("<%s> is an illegal file name"),
			  camera_opt->answer);
	else
	    strcpy(camera, camera_opt->answer);
    }

    /* I_put_camera (camera); */
    I_put_group_camera(group, camera);

    G_message(
	_("Group [%s] in location [%s] mapset [%s] now has camera file [%s]"),
	  group, location, mapset, camera);
#ifdef DEBUG
    /* slight pause before the screen is cleared */
    sleep(3);
#endif

    /* show the camera info for modification */
    if (I_find_camera(camera)) {
	have_old = 1;
	I_get_cam_info(camera, &cam_info);
    }
    mod_cam_info(have_old, &cam_info);
    I_put_cam_info(camera, &cam_info);

    exit(EXIT_SUCCESS);
}
