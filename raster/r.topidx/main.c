/*
 * r.topidx: creates topographic index map from elevation map.
 * 	     Based on GRIDATB.FOR.
 *
 * GRIDATB.FOR Author: Keith Beven <k.beven@lancaster.ac.uk>
 *
 *	Copyright (C) 2000 by the GRASS Development Team
 *	Author: Huidae Cho <grass4u@gmail.com>
 *		Hydro Laboratory, Kyungpook National University
 *		South Korea
 *
 *	This program is free software under the GPL (>=v2)
 *	Read the file COPYING coming with GRASS for details.
 *
 */

#define	MAIN
#include "local_proto.h"
#undef	MAIN

#include "gis.h"
#include "glocale.h"

int
main(int argc, char **argv)
{
	struct GModule *module;
	struct
	{
		struct	Option	*input;
		struct	Option	*output;
	} params;

	struct
	{
		struct	Flag	*verbose;
	} flags;

	G_gisinit(argv[0]);

	module = G_define_module();
        module->description =
		_("Creates topographic index, ln(a/tan(beta)), map from elevation map.");

	params.input			= G_define_option();
	params.input->key		= "input";
	params.input->description	= _("Elevation map");
	params.input->type		= TYPE_STRING;
	params.input->required		= YES;
	params.input->gisprompt		= "old,cell,raster";

	params.output			= G_define_option();
	params.output->key		= "output";
	params.output->description	= _("Topographic index ln(a/tanB) map");
	params.output->type		= TYPE_STRING;
	params.output->required		= YES;
	params.output->gisprompt	= "new,cell,raster";

	flags.verbose			= G_define_flag();
	flags.verbose->key		= 'v';
	flags.verbose->description	= _("Output verbosely");

	if(G_parser(argc, argv)){
	        exit(-1);
	}

	/* Make sure that the current projection is not lat/long */
	if ((G_projection() == PROJECTION_LL))
		G_fatal_error ("lat/long databases not supported by r.topidx. Please reproject map first.");

	iname   = params.input->answer;
	mapset  = G_find_cell2 (iname, "");
	oname   = params.output->answer;
	verbose = flags.verbose->answer;

	if(check_ready())
		exit(-1);

	G_get_window(&window);

	getcells();
	initialize();
	atanb();
	putcells();

	exit(0);
}

