/*
 * MODULE:       v.in.dxf
 *
 * AUTHOR(S):    Original written by Chuck Ehlschlaeger, 6/1989
 *               Revised by Dave Gerdes, 12/1989
 *               US Army Construction Engineering Research Lab
 *
 *               Contribution:
 *               Benjamin Horner-Johnson <ben earth.nwu.edu>
 *               Michel Wurtz <michel.wurtz teledetection.fr>
 *               Jacques Bouchard <bouchard onera.fr>
 *               J Moorman
 *               Tom Howard, National Park Service GIS division
 *
 *               Rewrite for GRASS 6:
 *               Huidae Cho <grass4u gmail.com>
 *
 * PURPOSE:      Import DXF file
 *
 * COPYRIGHT:    (C) 1999-2006 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 */

#define _MAIN_C_
#include <stdlib.h>
#include <grass/gis.h>
#include <grass/glocale.h>
#include "global.h"

int main(int argc, char *argv[])
{
    struct dxf_file *dxf;
    struct Map_info *Map;
    char *output = NULL;
    int ret;

    struct GModule *module;
    struct
    {
	struct Flag *list;
	struct Flag *extent;
	struct Flag *table;
	struct Flag *topo;
	struct Flag *invert;
	struct Flag *one_layer;
	struct Flag *frame;
    } flag;
    struct
    {
	struct Option *input;
	struct Option *output;
	struct Option *layers;
    } opt;

    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("vector, import");
    module->description =
	_("Converts files in DXF format to GRASS vector map format.");

    flag.extent = G_define_flag();
    flag.extent->key = 'e';
    flag.extent->description = _("Ignore the map extent of DXF file");

    flag.table = G_define_flag();
    flag.table->key = 't';
    flag.table->description = _("Do not create attribute tables");

    flag.topo = G_define_flag();
    flag.topo->key = 'b';
    flag.topo->description = _("Do not build topology");

    flag.frame = G_define_flag();
    flag.frame->key = 'f';
    flag.frame->description = _("Import polyface meshes as 3D wire frame");

    flag.list = G_define_flag();
    flag.list->key = 'l';
    flag.list->description = _("List available layers and exit");
    flag.list->guisection = _("DXF layers");

    flag.invert = G_define_flag();
    flag.invert->key = 'i';
    flag.invert->description =
	_("Invert selection by layers (don't import layers in list)");
    flag.invert->guisection = _("DXF layers");

    flag.one_layer = G_define_flag();
    flag.one_layer->key = '1';
    flag.one_layer->description = _("Import all objects into one layer");
    flag.one_layer->guisection = _("DXF layers");

    opt.input = G_define_standard_option(G_OPT_F_INPUT);
    opt.input->description = _("Name of input DXF file");

    opt.output = G_define_standard_option(G_OPT_V_OUTPUT);
    opt.output->required = NO;

    opt.layers = G_define_option();
    opt.layers->key = "layers";
    opt.layers->type = TYPE_STRING;
    opt.layers->required = NO;
    opt.layers->multiple = YES;
    opt.layers->description = _("List of layers to import");
    opt.layers->guisection = _("DXF layers");

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    flag_list = flag.list->answer;
    flag_extent = flag.extent->answer;
    flag_table = flag.table->answer;
    flag_invert = flag.invert->answer;
    flag_one_layer = flag.one_layer->answer;
    flag_frame = flag.frame->answer;

    /* open DXF file */
    if (!(dxf = dxf_open(opt.input->answer)))
	G_fatal_error(_("Unable to open DXF file <%s>"), opt.input->answer);

    if (flag_list) {
	num_layers = 0;
	layers = NULL;
	Map = NULL;
    }
    else {
	/* make vector map name SQL compliant */
	if (opt.output->answer) {
	    output = G_store(opt.output->answer);
	}
	else {
	    char *p, *p2;

	    if ((p = G_rindex(dxf->name, '/')))
		p++;
	    else
		p = dxf->name;
	    output = G_store(p);
	    if ((p2 = G_rindex(p, '.')))
		output[p2 - p] = 0;
	}
	{
	    char *p;

	    for (p = output; *p; p++)
		if (*p == '.')
		    *p = '_';
	}

	layers = opt.layers->answers;

	if (!G_check_overwrite(argc, argv) &&
	    G_find_vector2(output, G_mapset())) {
	    G_fatal_error(_("Option <%s>: <%s> exists."), opt.output->key,
			  output);
	}

	if (Vect_legal_filename(output) < 0)
	    G_fatal_error(_("Use '%s' option to change vector map name"),
			  opt.output->key);

	/* create vector map */
	Map = (struct Map_info *)G_malloc(sizeof(struct Map_info));
	if (Vect_open_new(Map, output, 1) < 0)
	    G_fatal_error(_("Unable to create vector map <%s>"), output);

	Vect_set_map_name(Map, output);

	Vect_hist_command(Map);
    }

    /* import */
    ret = dxf_to_vect(dxf, Map);

    dxf_close(dxf);

    if (flag_list)
	init_list();
    else {
	Vect_close(Map);

	if (ret) {
	    if (Vect_open_old(Map, output, G_mapset())) {
		if (!flag_topo)
		    if (!Vect_build(Map))
			G_warning(_("Building topology failed"));
		Vect_close(Map);
	    }
	}
	else {
	    Vect_delete(output);
	    G_fatal_error(_("Failed to import DXF file!"));
	}

	G_free(output);
	G_free(Map);
    }

    G_done_msg(" ");

    exit(EXIT_SUCCESS);
}
