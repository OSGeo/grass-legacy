/*
 * r.in.gridatb: Imports GRIDATB.FOR map file (TOPMODEL) into GRASS raster map
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
main(argc,argv)
	int	argc;
	char	**argv;
{
	struct
	{
		struct	Option	*input;
		struct	Option	*output;
	} params;

	struct
	{
		struct	Flag	*overwr;
	} flags;
	struct GModule *module;
	

	G_gisinit(argv[0]);
	
	/* Set description */
	module              = G_define_module();
	module->description = 
	_("Imports GRIDATB.FOR map file (TOPMODEL) into GRASS raster map");

	params.input			= G_define_option();
	params.input->key		= "input";
	params.input->description	= _("GRIDATB i/o map file");
	params.input->type		= TYPE_STRING;
	params.input->required		= YES;

	params.output			= G_define_option();
	params.output->key		= "output";
	params.output->description	= _("Output map");
	params.output->type		= TYPE_STRING;
	params.output->required		= YES;
	params.output->gisprompt	= "new,cell,raster";

	flags.overwr			= G_define_flag();
	flags.overwr->key		= 'o';
	flags.overwr->description	= _("Overwrite output map");

	if(G_parser(argc, argv)){
	        exit(-1);
	}

	file   = params.input->answer;
	oname  = params.output->answer;
	overwr = flags.overwr->answer;

	mapset = G_mapset();

	if(check_ready()){
		exit(-1);
	}

	rdwr_gridatb();

	exit(0);
}

