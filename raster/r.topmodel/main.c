/*
 * r.topmodel: simulates TOPMODEL.
 *
 *    Contact: Huidae Cho <hdcho@geni.knu.ac.kr>
 *
 * $Id$
 *
 * $Log$
 * Revision 1.2  2000-08-20 06:35:19  cho
 * cosmetics
 *
 * Revision 1.1  2000/08/20 05:57:21  cho
 * Huidae Cho <hdcho@geni.knu.ac.kr>: TOPMODEL simulation and other stuff
 *
 *
 */

#define	MAIN
#include "local_proto.h"
#undef	MAIN


int
main(argc, argv)
	int	argc;
	char	**argv;
{
	struct	Cell_head	cellhd;
	char	*hdmap;

	struct
	{
		struct	Option	*elev;
		struct	Option	*basin;
		struct	Option	*fill;
		struct	Option	*dir;
		struct	Option	*belev;
		struct	Option	*topidx;
		struct	Option	*nidxclass;
		struct	Option	*idxstats;
		struct	Option	*params;
		struct	Option	*input;
		struct	Option	*output;
		struct	Option	*timestep;
		struct	Option	*idxclass;
	} param;

	struct
	{
		struct	Flag	*input;
		struct	Flag	*overwr;
		struct	Flag	*wide;
	} flag;


	/* Parameter definitions */
	param.elev			= G_define_option();
	param.elev->key			= "elevation";
	param.elev->description		=
		"(i)   Elevation map";
	param.elev->type		= TYPE_STRING;
	param.elev->required		= NO;
	param.elev->gisprompt		= "old,cell,raster";

	param.basin			= G_define_option();
	param.basin->key		= "basin";
	param.basin->description	=
		"(i)   Basin map created by r.water.outlet";
	param.basin->type		= TYPE_STRING;
	param.basin->required		= NO;
	param.basin->gisprompt		= "old,cell,raster";

	param.fill			= G_define_option();
	param.fill->key			= "depressionless";
	param.fill->description		=
		"(o)   Depressionless elevation map";
	param.fill->type		= TYPE_STRING;
	param.fill->required		= NO;
	param.fill->gisprompt		= "new,cell,raster";

	param.dir			= G_define_option();
	param.dir->key			= "direction";
	param.dir->description		=
		"(o)   Direction map with depressionless elevation map";
	param.dir->type			= TYPE_STRING;
	param.dir->required		= NO;
	param.dir->gisprompt		= "new,cell,raster";

	param.belev			= G_define_option();
	param.belev->key		= "belevation";
	param.belev->description	=
		"(i/o) Basin elevation map (extracted)";
	param.belev->type		= TYPE_STRING;
	param.belev->required		= NO;
	param.belev->gisprompt		= "new,cell,raster";

	param.topidx			= G_define_option();
	param.topidx->key		= "topidx";
	param.topidx->description	=
		"(i/o) Topographic index ln(a/tanB) map (extracted)";
	param.topidx->type		= TYPE_STRING;
	param.topidx->required		= NO;
	param.topidx->gisprompt		= "new,cell,raster";

	param.nidxclass			= G_define_option();
	param.nidxclass->key		= "nidxclass";
	param.nidxclass->description	=
		"(i)   Number of topographic index classes";
	param.nidxclass->type		= TYPE_INTEGER;
	param.nidxclass->required	= NO;
	param.nidxclass->answer		= "30";

	param.idxstats			= G_define_option();
	param.idxstats->key		= "idxstats";
	param.idxstats->description	=
		"(i/o) Topographic index statistics file";
	param.idxstats->type		= TYPE_STRING;
	param.idxstats->required	= YES;

	param.params			= G_define_option();
	param.params->key		= "parameters";
	param.params->description	=
		"(i)   Parameters file";
	param.params->type		= TYPE_STRING;
	param.params->required		= YES;

	param.input			= G_define_option();
	param.input->key		= "input";
	param.input->description	=
		"(i)   Input file";
	param.input->type		= TYPE_STRING;
	param.input->required		= YES;

	param.output			= G_define_option();
	param.output->key		= "output";
	param.output->description	=
		"(o)   Output file";
	param.output->type		= TYPE_STRING;
	param.output->required		= YES;

	param.timestep			= G_define_option();
	param.timestep->key		= "timestep";
	param.timestep->description	=
		"      Time step for output";
	param.timestep->type		= TYPE_INTEGER;
	param.timestep->required	= NO;

	param.idxclass			= G_define_option();
	param.idxclass->key		= "idxclass";
	param.idxclass->description	=
		"      Topographic index class for output";
	param.idxclass->type		= TYPE_INTEGER;
	param.idxclass->required	= NO;


	/* Flag definitions */
	flag.input			= G_define_flag();
	flag.input->key			= 'i';
	flag.input->description		=
		"Input data given for (i/o)";

	flag.overwr			= G_define_flag();
	flag.overwr->key		= 'o';
	flag.overwr->description	=
		"Overwrite outputs";

	flag.wide			= G_define_flag();
	flag.wide->key			= 'w';
	flag.wide->description		=
		"Wide output";


	/* Initialize GRASS and parse command line */
	G_gisinit(argv[0]);

	if(G_parser(argc, argv)){
	        exit(-1);
	}

	/* Store given parameters and flags */
	map.elev	= param.elev->answer;
	map.basin	= param.basin->answer;
	map.belev	= param.belev->answer;
	map.fill	= param.fill->answer;
	map.dir		= param.dir->answer;
	map.topidx	= param.topidx->answer;
	file.idxstats	= param.idxstats->answer;
	file.params	= param.params->answer;
	file.input	= param.input->answer;
	file.output	= param.output->answer;

	misc.nidxclass	= atoi(param.nidxclass->answer);

	if(!param.timestep->answer)
		param.timestep->answer = "0";
	if(!param.idxclass->answer)
		param.idxclass->answer = "0";

	misc.timestep	= atoi(param.timestep->answer);
	misc.idxclass	= atoi(param.idxclass->answer);

	flg.input	= flag.input->answer;
	flg.overwr	= flag.overwr->answer;
	flg.wide	= flag.wide->answer;


	gisbase = G_gisbase();
	mapset  = G_mapset();


	/* Check run conditions */
	if(check_ready())
		exit(1);


	/* Adjust cell header */
	hdmap = NULL;
	if(!flg.input){
		hdmap = map.elev;
	}else{
		if(map.belev)
			hdmap = map.belev;
		else if(map.topidx)
			hdmap = map.topidx;
	}

	if(hdmap){
		sprintf(buf, "%s/bin/g.region rast=%s", gisbase, hdmap);
		G_system(buf);
	}


	/* Create required maps */
	if(!flg.input){
		if(map.fill)
			depressionless();
		basin_elevation();
	}

	top_index();


	/* Read required files */
	read_inputs();


	/* Implement TOPMODEL */
	topmodel();


	/* Write outputs */
	write_outputs();


	exit(0);
}

