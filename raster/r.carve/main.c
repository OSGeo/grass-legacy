/* Written by Bill Brown, UIUC GIS Laboratory */
/* Ported to GRASS6 by Brad Douglas <rez@touchofmadness.com> */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
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


/* function prototypes */
static int init_projection(struct Cell_head *window, int *wrap_ncols);


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
    int wrap = 0;
    struct Map_info Map;
    struct Map_info outMap;
    RASTER_MAP_TYPE rtype;
    struct Cell_head win;

    /* start GIS engine */
    G_gisinit(argv[0]);

    module = G_define_module();
    module->description = _("Takes vector stream data, transforms it "
                    "to raster, and subtracts depth from the output DEM");

    /* setup lat/lon projection and distance calculations */
    init_projection(&win, &wrap);

    /* default width - one cell at center */
    parm.swidth = G_distance((win.east + win.west) / 2,
                             (win.north + win.south) / 2, 
                             ((win.east + win.west) / 2) + win.ew_res,
                             (win.north + win.south) / 2);
    sprintf(defwidth, "%.6lf", parm.swidth);

    parm.inrast = G_define_standard_option(G_OPT_R_INPUT);
    parm.inrast->key           = "rast";
    parm.inrast->description   = _("Name of input raster elevation map");

    parm.invect = G_define_standard_option(G_OPT_V_INPUT);
    parm.invect->key           = "vect";
    parm.invect->description   = _("Name of vector input map containing stream");

    parm.outrast = G_define_standard_option(G_OPT_R_OUTPUT);

    parm.outvect = G_define_standard_option(G_OPT_V_OUTPUT);
    parm.outvect->key          = "points";
    parm.outvect->required     = NO;
    parm.outvect->description  = _("Name of output vector map for adjusted stream points");

    parm.width = G_define_option();
    parm.width->key            = "width";
    parm.width->type           = TYPE_DOUBLE;
    parm.width->description    = _("Width of stream (in meters)");
    parm.width->answer         = defwidth;

    parm.depth = G_define_option();
    parm.depth->key            = "depth";
    parm.depth->type           = TYPE_DOUBLE;
    parm.depth->description    = _("Additional stream depth");
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

    G_check_input_output_name(parm.inrast->answer, parm.outrast->answer, GR_FATAL_EXIT);

    /* convert text params to double format */
    if (sscanf(parm.width->answer, "%lf", &parm.swidth) != 1)
        G_warning(_("invalid width value - using default."));

    if (sscanf(parm.depth->answer, "%ld", &parm.sdepth) != 1)
    {
        G_warning(_("invalid depth value - using default."));
        parm.sdepth = 0.0;
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


static int init_projection(struct Cell_head *window, int *wrap_ncols)
{
#if 0
    double a, e2;
#endif

    G_get_set_window(window);

    if (((window->west == (window->east - 360.0)) ||
        (window->east == (window->west - 360.0))) &&
        (G_projection() == PROJECTION_LL))
    {
#if 0
        G_get_ellipsoid_parameters(&a, &e2);
        G_begin_geodesic_distance(a, e2);

        /* add 1.1, not 1.0, to ensure that we round up */
        *wrap_ncols = (360.0 - (window->east - window->west)) / window->ew_res + 1.1;
#else
        G_fatal_error(_("lat/lon projection not supported at this time."));
#endif
    } else {
        *wrap_ncols = 0;
    }

    G_begin_distance_calculations();

    return 0;
}
