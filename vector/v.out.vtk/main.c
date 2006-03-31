/*
 *
 ****************************************************************************
 *
 * MODULE:     v.out.vtk  
 * AUTHOR(S):  Soeren Gebbert
 *
 * PURPOSE:    v.out.vtk: writes ASCII VTK file
 *             this module is based on v.out.ascii
 * COPYRIGHT:  (C) 2000 by the GRASS Development Team
 *
 *             This program is free software under the GNU General Public
 *              License (>=v2). Read the file COPYING that comes with GRASS
 *              for details.
 *
 ****************************************************************************
 */
/*  @(#)b_a_dig.c       2.1  6/26/87  */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/Vect.h>
#include <grass/glocale.h>
#include "local_proto.h"
#include <string.h>

int main(int argc, char *argv[])
{
    FILE *ascii;
    struct Option *input, *output, *feature_opt, *dp_opt, *layer_opt;
    int feature, dp;
    struct Map_info Map;
    struct GModule *module;
    int layer;

    G_gisinit(argv[0]);

    module = G_define_module();
    module->description =
	_("Convert a GRASS binary vector map to VTK ASCII output");

    input = G_define_standard_option(G_OPT_V_INPUT);

    output = G_define_option();
    output->key = "output";
    output->type = TYPE_STRING;
    output->required = NO;
    output->multiple = NO;
    output->gisprompt = "new_file,,output";
    output->description = _("Path to resulting VTK file.");

    feature_opt = G_define_option();
    feature_opt->key = "feature";
    feature_opt->type = TYPE_STRING;
    feature_opt->required = NO;
    feature_opt->multiple = NO;
    feature_opt->options = "point,kernel,centroid,line,boundary,area,face";
    feature_opt->answer = "line";
    feature_opt->description = _("Feature that will be exported");

    dp_opt = G_define_option();
    dp_opt->key = "dp";
    dp_opt->type = TYPE_INTEGER;
    dp_opt->required = NO;
    dp_opt->description =
	_("Number of significant digits (floating point only)");

    layer_opt = G_define_option();
    layer_opt->key = "layer";
    layer_opt->type = TYPE_INTEGER;
    layer_opt->required = NO;
    layer_opt->answer = "1";
    layer_opt->description = _("Layer number");

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    /*The default */
    feature = GV_LINE;

    if (strcmp(feature_opt->answer, "point") == 0)
	feature = GV_POINT;
    if (strcmp(feature_opt->answer, "kernel") == 0)
	feature = GV_KERNEL;
    if (strcmp(feature_opt->answer, "line") == 0)
	feature = GV_LINE;
    if (strcmp(feature_opt->answer, "boundary") == 0)
	feature = GV_BOUNDARY;
    if (strcmp(feature_opt->answer, "centroid") == 0)
	feature = GV_CENTROID;
    if (strcmp(feature_opt->answer, "area") == 0)
	feature = GV_AREA;
    if (strcmp(feature_opt->answer, "face") == 0)
	feature = GV_FACE;

    /*We need level 2 functions */
    Vect_set_open_level(2);
    Vect_open_old(&Map, input->answer, "");

    if (output->answer) {
	ascii = fopen(output->answer, "w");
	if (ascii == NULL) {
	    G_fatal_error(_("Cannot open file [%s]"), output->answer);
	}
    }
    else {
	ascii = stdout;
    }

    /*The precision of the output */
    if (dp_opt->answer) {
	if (sscanf(dp_opt->answer, "%d", &dp) != 1)
	    G_fatal_error(_("failed to interpret dp as an integer"));
	if (dp > 8 || dp < 0)
	    G_fatal_error(_("dp has to be from 0 to 8"));
    }
    else {
	dp = 8;			/*This value is taken from the lib settings in G_feature_easting */
    }

    /*The Layer */
    if (layer_opt->answer) {
	if (sscanf(layer_opt->answer, "%d", &layer) != 1)
	    G_fatal_error(_("failed to interpret layer as an integer"));
    }
    else {
	layer = 1;
    }

    /*Write the header */
    writeHead(ascii, &Map);
    /*Write the geometries features */
    writeVTK(ascii, &Map, layer, feature, dp);

    if (ascii != NULL)
	fclose(ascii);

    Vect_close(&Map);

    exit(EXIT_SUCCESS);
}
