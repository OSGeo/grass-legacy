/*
 * r.in.gridatb: imports GRIDATB.FOR map file. 
 *
 * GRIDATB.FOR Author: Keith Beven <k.beven@lancaster.ac.uk>
 *
 * $Id$
 *
 *	Copyright (C) 2000 by the GRASS Development Team
 *	Author: Huidae Cho <hdcho@geni.knu.ac.kr>
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


	params.input			= G_define_option();
	params.input->key		= "input";
	params.input->description	= "GRIDATB i/o map file";
	params.input->type		= TYPE_STRING;
	params.input->required		= YES;

	params.output			= G_define_option();
	params.output->key		= "output";
	params.output->description	= "Output map";
	params.output->type		= TYPE_STRING;
	params.output->required		= YES;
	params.output->gisprompt	= "new,cell,raster";

	flags.overwr			= G_define_flag();
	flags.overwr->key		= 'o';
	flags.overwr->description	= "Overwrite output map";


	G_gisinit(argv[0]);

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

