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

#include <stdlib.h>
#include <stdio.h>

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
		struct	Flag	*overwr;
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

	flags.overwr			= G_define_flag();
	flags.overwr->key		= 'o';
	flags.overwr->description	= _("Overwrite output map");

	flags.verbose			= G_define_flag();
	flags.verbose->key		= 'v';
	flags.verbose->description	= _("Output verbosely");

	if(G_parser(argc, argv)){
	        exit(-1);
	}

	iname   = params.input->answer;
	mapset  = G_find_cell2 (iname, "");
	oname   = params.output->answer;
	overwr  = flags.overwr->answer;
	verbose = flags.verbose->answer;

	if(check_ready())
		exit(-1);

	G_get_cellhd(iname,mapset,&cellhd);
	if(adjcellhd(&cellhd))
		exit(-1);

	getcells();
	initialize();
	atanb();
	putcells();

	exit(0);
}

