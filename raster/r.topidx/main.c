/*
 * r.topidx: creates topographic index map from elevation map.
 *
 *  Contact: Huidae Cho <hdcho@geni.knu.ac.kr>
 *
 * $Id$
 *
 * $Log$
 * Revision 1.1  2000-08-20 05:57:16  cho
 * Huidae Cho <hdcho@geni.knu.ac.kr>: TOPMODEL simulation and other stuff
 *
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
		struct	Flag	*verbose;
	} flags;


	params.input			= G_define_option();
	params.input->key		= "input";
	params.input->description	= "Elevation map";
	params.input->type		= TYPE_STRING;
	params.input->required		= YES;
	params.input->gisprompt		= "old,cell,raster";

	params.output			= G_define_option();
	params.output->key		= "output";
	params.output->description	= "Topographic index ln(a/tanB) map";
	params.output->type		= TYPE_STRING;
	params.output->required		= YES;
	params.output->gisprompt	= "new,cell,raster";

	flags.overwr			= G_define_flag();
	flags.overwr->key		= 'o';
	flags.overwr->description	= "Overwrite output map";

	flags.verbose			= G_define_flag();
	flags.verbose->key		= 'v';
	flags.verbose->description	= "Output verbosely";


	G_gisinit(argv[0]);

	if(G_parser(argc, argv)){
	        exit(-1);
	}

	mapset  = G_mapset();
	iname   = params.input->answer;
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

