#include "local_proto.h"


int
run(cmd)
	char	*cmd;
{
	int	retval;


	if(G_system(cmd)){
		fprintf(stderr, "Failed\n");
		retval=1;
	}else{
		fprintf(stderr, "OK\n");
		retval=0;
	}


	return(retval);
}


void
gregion(void)
{
	char	*hdmap;


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
		sprintf(buf, "%s/bin/g.region rast=%s > /dev/null",
				gisbase, hdmap);
		fprintf(stderr, "g.region rast=%s ... ",
				hdmap);

		if(run(buf))
			exit(1);
	}
}


void
depressionless(void)
{
	sprintf(buf, "%s/bin/r.fill.dir "
		     "input=%s elev=%s dir=%s type=grass > /dev/null", 
			gisbase, map.elev, map.fill, map.dir);
	fprintf(stderr, "r.fill.dir input=%s elev=%s dir=%s type=grass ... ",
			map.elev, map.fill, map.dir);

	if(run(buf))
		exit(1);

	map.elev = map.fill;


	return;
}


void
basin_elevation(void)
{
	sprintf(buf, "%s/bin/r.mapcalc "
		     "'%s = if(isnull(%s), %s, %s)' > /dev/null",
			gisbase, map.belev, map.basin, map.basin, map.elev);
	fprintf(stderr, "r.mapcalc '%s = if(isnull(%s), %s, %s)' ... ",
			map.belev, map.basin, map.basin, map.elev);

	if(run(buf))
		exit(1);


	return;
}


void
top_index(void)
{
	if(map.belev){
		sprintf(buf, "%s/bin/r.topidx "
			     "input=%s output=%s > /dev/null",
				gisbase, map.belev, map.topidx);
		fprintf(stderr, "r.topidx input=%s output=%s ... ", 
				map.belev, map.topidx);

		if(run(buf))
			exit(1);
	}

	if(map.topidx){
		sprintf(buf, "%s/scripts/r.avgstats "
			     "map=%s nsteps=%d > %s",
				gisbase, map.topidx, misc.nidxclass,
				file.idxstats);
		fprintf(stderr, "r.avgstats map=%s nsteps=%d > %s ... ",
				map.topidx, misc.nidxclass, file.idxstats);

		if(run(buf))
			exit(1);
	}


	return;
}

