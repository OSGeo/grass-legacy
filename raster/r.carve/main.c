/* Written by Bill Brown, UIUC GIS Laboratory */
/* Ported to GRASS6 by Brad Douglas <rez@touchofmadness.com> */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <grass/gis.h>
#include <grass/glocale.h>
#include "enforce.h"


/*
    Note: Use rast input type for rast output 
    Read vect file, 
    for each line,
	use a shadow line struct to represent stream profile,
	    where x is distance along stream and y is elevation,
	    adding each point to lobf as it's created.
	find trend using lobf
	from high to low, lower any points forming dams
	    when next pnt elev increases,
	    find next point <= than last confirmed pt
	    just use linear interp for now
        write line to new raster  
	    Use orig line struct for XYs, shadow struct Y for cell val
	    if new raster already has value there, use lower?
		actually probably want to use val for trunk stream 
		and then verify branch in reverse
		- for now maybe create a conflict map
    After that's working, add width to lines? 
	or use r.grow
*/


int main(int argc, char **argv) 
{
    struct GModule *module;
    struct {
        struct Option *inrast, *invect, *outrast, *outvect, *width, *depth;
        struct Flag *quiet, *noflat;
        double swidth, sdepth;
    } parm;
    int retval;
    char defwidth[80];

    char *vmapset, *rmapset; 
    int infd, outfd;
    struct Map_info Map;
    struct Map_info outMap;
    RASTER_MAP_TYPE rtype;
    struct Cell_head wind;

    /* start GIS engine */
    G_gisinit(argv[0]);

    module = G_define_module();
    module->description = _("Takes vector stream data, transforms it "
                    "to raster, and subtracts depth from the output DEM");

    G_get_set_window(&wind);
    G_begin_distance_calculations();

    /* default width one cell at center */
    parm.swidth = G_distance((wind.east + wind.west) / 2,
                             (wind.north + wind.south) / 2, 
                             ((wind.east + wind.west) / 2) + wind.ew_res,
                             (wind.north + wind.south) / 2);
    sprintf(defwidth, "%.6lf", parm.swidth);

    parm.inrast = G_define_standard_option(G_OPT_R_INPUT);
    parm.inrast->key           = "rast";
    parm.inrast->description   = _("Raster input elevation map");

    parm.invect = G_define_standard_option(G_OPT_V_INPUT);
    parm.invect->key           = "vect";
    parm.invect->description   = _("Vector input map containing stream");

    parm.outrast = G_define_standard_option(G_OPT_R_OUTPUT);

    parm.outvect = G_define_standard_option(G_OPT_V_OUTPUT);
    parm.outvect->key          = "points";
    parm.outvect->required     = NO;
    parm.outvect->description  = _("Vector output map for adjusted stream points");

    parm.width = G_define_option();
    parm.width->key            = "width";
    parm.width->type           = TYPE_DOUBLE;
    parm.width->description    = _("Width of streams (in meters)");
    parm.width->answer         = defwidth;

    parm.depth = G_define_option();
    parm.depth->key            = "depth";
    parm.depth->type           = TYPE_DOUBLE;
    parm.depth->description    = _("Additional depth");
    parm.depth->answer         = "0.0";

    parm.quiet = G_define_flag();
    parm.quiet->key          = 'q';
    parm.quiet->description  = _("Quiet - Do not show progress");

    parm.noflat = G_define_flag();
    parm.noflat->key         = 'n';
    parm.noflat->description = _("No flat areas allowed in flow direction");

    /* parse options */
    if (G_parser(argc, argv))
        exit(EXIT_FAILURE);

    /* convert text params to double format */
    if (parm.width->answer) {
        if (sscanf(parm.width->answer, "%lf", &parm.swidth) != 1) {
            G_warning(_("invalid width value - using default"));
            sscanf(defwidth, "%lf", &parm.swidth);
        }
    }

    if (parm.depth->answer) {
        if (sscanf(parm.depth->answer, "%lf", &parm.sdepth) != 1) {
            G_warning(_("invalid depth value - using default"));
            parm.sdepth = 0.;
        }
    }

    /* open input files */
    if ((vmapset = G_find_vector2(parm.invect->answer, "")) == NULL)
        G_fatal_error(_("Vector map <%s> not found"), parm.invect->answer);

    Vect_set_open_level(2);
    Vect_open_old(&Map, parm.invect->answer, vmapset);

    if ((rmapset = G_find_file2("cell", parm.inrast->answer, "")) == NULL)
        G_fatal_error(_("unable to find raster file %s"), parm.inrast->answer);

    if ((infd = G_open_cell_old(parm.inrast->answer, rmapset)) == -1)
        G_fatal_error(_("unable to open cellfile for [%s]"), parm.inrast->answer);

    /* open new map for output */
    rtype = G_raster_map_type(parm.inrast->answer, rmapset);
    if ((outfd = G_open_raster_new(parm.outrast->answer,  rtype)) < 0)
        G_fatal_error(_("unable to open cellfile for [%s]"), parm.outrast->answer);

    /* if specified, open vector for output */
    if (parm.outvect->answer)
        open_new_vect(&outMap, parm.outvect->answer);

    retval = enforce_downstream(infd, outfd, parm.outvect->answer, &Map,
                &outMap, rtype, parm.swidth, parm.sdepth, 
                parm.noflat->answer, parm.quiet->answer);

    G_close_cell(infd);
    G_close_cell(outfd);
    close_vect(&Map, 0);

    if (parm.outvect->answer)
        close_vect(&outMap, 1);

    /* write command line to history file */
    update_history(parm.outrast->answer);

    if (!parm.quiet->answer)
	G_done_msg(G_recreate_command());

    return retval;
}
