#include "local_proto.h"

#define	GIVEN_TI(name)	fprintf(fp, "%-15s %15d       # %s\n",		\
					(name),				\
					(misc.timestep ?		\
					 (misc.idxclass ?		\
					  1				\
					  : misc.nidxclass)		\
					 : input.ntimestep),		\
					(misc.timestep ?		\
					 (misc.idxclass ?		\
					  "1"				\
					  : "nidxclass")		\
					 : "input.ntimestep"));


void
get_line(fp, buffer)
	FILE	*fp;
	char	*buffer;
{
	char	*str;


	buffer[0] = 0;
	fscanf(fp, "%[^\n]", buffer);
	getc(fp);

	if((str = (char *) strchr(buffer, '#')))
		*str = 0;


	return;
}


void
read_inputs(void)
{
	FILE	*fp;
	char	buffer[BUFSIZE];
	int	i;


	/* Read topographic index statistics file */
	fp = fopen(file.idxstats, "r");
	idxstats.atb    = (double *) malloc(misc.nidxclass * sizeof(double));
	idxstats.Aatb_r = (double *) malloc(misc.nidxclass * sizeof(double));

	misc.ncell = 0;

	for(i=0; i<misc.nidxclass && !feof(fp); ){
		get_line(fp, buf);

		if(sscanf(buf, "%lf %lf",
				&(idxstats.atb[i]), &(idxstats.Aatb_r[i])) == 2)
			misc.ncell += (int) idxstats.Aatb_r[i++];
	}

	misc.nidxclass = i;
	fclose(fp);

	for(i=0; i<misc.nidxclass; i++)
		idxstats.Aatb_r[i] /= (double) misc.ncell;


	/* Read parameters file */
	fp = fopen(file.params, "r");

	for(; !feof(fp); ){
		buffer[0]=0;
		get_line(fp, buf);
		sprintf(buffer, " %s", buf);
		sscanf(buffer, "%*[ \t]%[^\0]", buf);

		i=strlen(buf)-1;
		for(;i>=0;i--){
			if(buf[i]!=' ' && buf[i]!='\t'){
				buf[i+1]=0;
				break;
			}
		}
		if(i>=0)
			break;
	}
	params.name = G_store(buf);

	for(; !feof(fp); ){
		get_line(fp, buf);

		if(sscanf(buf, "%lf", &(params.A)) == 1)
			break;
	}

	for(; !feof(fp); ){
		get_line(fp, buf);

		if(sscanf(buf, "%lf %lf %lf %lf %lf %lf %lf %lf",
				&(params.qs0),	&(params.lnTe),
				&(params.m),	&(params.Sr0),
				&(params.Srmax),&(params.td),
				&(params.vch),	&(params.vr)) == 8)
			break;
	}

	for(; !feof(fp); ){
		get_line(fp, buf);

		if(sscanf(buf, "%d %lf %lf %lf",
				&(params.infex),&(params.K),
				&(params.psi),	&(params.dtheta)) == 4)
			break;
	}

	for(; !feof(fp); ){
		get_line(fp, buf);

		if(sscanf(buf, "%d", &(params.nch)) == 1)
			break;
	}

	params.d    = (double *) malloc(params.nch * sizeof(double));
	params.Ad_r = (double *) malloc(params.nch * sizeof(double));

	for(i=0; i<params.nch && !feof(fp); ){
		get_line(fp, buf);

		if(sscanf(buf, "%lf %lf",
				&(params.d[i]), &(params.Ad_r[i])) == 2)
			i++;
	}

	params.nch = i;
	fclose(fp);


	/* Read input file */
	fp = fopen(file.input, "r");

	for(; !feof(fp); ){
		get_line(fp, buf);

		if(sscanf(buf, "%d %lf",
				&(input.ntimestep), &(input.dt)) == 2)
			break;
	}

	input.R_  = (double *) malloc(input.ntimestep * sizeof(double));
	input.Ep_ = (double *) malloc(input.ntimestep * sizeof(double));

	for(i=0; i<input.ntimestep && !feof(fp); ){
		get_line(fp, buf);

		if(sscanf(buf, "%lf %lf",
				&(input.R_[i]), &(input.Ep_[i])) == 2)
			i++;
	}

	input.ntimestep = i;
	fclose(fp);

	if(!(misc.timestep > 0 && misc.timestep < input.ntimestep + 1))
		misc.timestep = 0;
	if(!(misc.idxclass > 0 && misc.idxclass < misc.nidxclass  + 1))
		misc.idxclass = 0;


	return;
}


void
write_outputs(void)
{
	FILE	*fp;
	time_t	tloc;
	struct	tm	*ltime;
	int	st, et, si, ei;
	int	i, j;

	
	time(&tloc);
	ltime = localtime(&tloc);

	ltime->tm_year += (ltime->tm_year > 90 ? 1900 : 2000);
	ltime->tm_mon++;


	fp = fopen(file.output, "w");

	fprintf(fp, "# r.topmodel output file for \"%s\"\n",
							params.name);
#ifndef __CYGWIN__
	fprintf(fp, "# Run time: %.4d-%.2d-%.2d %.2d:%.2d:%.2d %s\n", 
			ltime->tm_year, ltime->tm_mon, ltime->tm_mday,
			ltime->tm_hour, ltime->tm_min, ltime->tm_sec,
			ltime->tm_zone);
#else
	fprintf(fp, "# Run time: %.4d-%.2d-%.2d %.2d:%.2d:%.2d\n", 
			ltime->tm_year, ltime->tm_mon, ltime->tm_mday,
			ltime->tm_hour, ltime->tm_min, ltime->tm_sec);
#endif
	fprintf(fp, "#\n");
	fprintf(fp, "# '_' suffix means time units in time step\n");
	fprintf(fp, "#\n");
	fprintf(fp, "# %-15s the number of non-null cells\n",
			"ncell:");
	fprintf(fp, "# %-15s the number of topographic index classes\n",
			"nidxclass:");
	fprintf(fp, "# %-15s the number of delay time step\n",
			"ndelay_:");
	fprintf(fp, "# %-15s the number of reach time step\n",
			"nreach_:");
	fprintf(fp, "# %-15s the areal average of ln(T0) = ln(Te)\n",
			"lnTe_:");
	fprintf(fp, "# %-15s the main channel routing velocity\n",
			"vch_:");
	fprintf(fp, "# %-15s the internal subcatchment routing velocity\n",
			"vr_:");
	fprintf(fp, "# %-15s the areal average of topographic index\n",
			"lambda:");
	fprintf(fp, "# %-15s the subsurface flow per unit area "
		    "at a soil surface\n",
			"_qs_:");
	fprintf(fp, "# %-15s the initial subsurface flow per unit area\n",
			"qs0_:");
	fprintf(fp, "#\n");
	fprintf(fp, "# %-15s the routing time step\n",
			"tch_::");
	fprintf(fp, "# %-15s the difference of area for each reach time step\n",
			"Add::");
	fprintf(fp, "# %-15s the total flow\n",
			"Qt_::");
	fprintf(fp, "# %-15s the total flow per unit area\n",
			"qt_::");
	fprintf(fp, "# %-15s the saturation overland flow per unit area\n",
			"qo_::");
	fprintf(fp, "# %-15s the subsurface flow per unit area\n",
			"qs_::");
	fprintf(fp, "# %-15s the vertical flux (or the drainage flux)\n",
			"qv_::");
	fprintf(fp, "# %-15s the mean saturation deficit in a watershed\n",
			"Sbar_::");
	if(params.infex){
		fprintf(fp, "# %-15s the infiltration rate\n",
			"f_::");
		fprintf(fp, "# %-15s the infiltration excess runoff\n",
			"fex_::");
	}

	if(misc.timestep || misc.idxclass){
		fprintf(fp, "#\n");
		fprintf(fp, "# %-15s the root zone storage deficit\n",
			"Srz_:::");
		fprintf(fp, "# %-15s the unsaturated (gravity drainage) zone "
			    "storage\n",
			"Suz_:::");
		fprintf(fp, "# %-15s the local saturated zone deficit due to "
			    "gravity drainage\n",
			"S_:::");
		fprintf(fp, "# %-15s the actual evapotranspiration\n",
			"Ea_:::");
		fprintf(fp, "# %-15s the excess flow from a fully saturated "
			    "area per unit area\n",
			"ex_:::");
	}

	fprintf(fp, "\n");

	fprintf(fp, "%-15s %15d\n",	"ncell:",	misc.ncell	);
	fprintf(fp, "%-15s %15d\n",	"nidxclass:",	misc.nidxclass	);
	fprintf(fp, "%-15s %15d\n",	"ndelay_:",	misc.ndelay_	);
	fprintf(fp, "%-15s %15d\n",	"nreach_:",	misc.nreach_	);
	fprintf(fp, "%-15s %15.5le\n",	"lnTe_:",	misc.lnTe_	);
	fprintf(fp, "%-15s %15.5le\n",	"vch_:",	misc.vch_	);
	fprintf(fp, "%-15s %15.5le\n",	"vr_:",		misc.vr_	);
	fprintf(fp, "%-15s %15.5le\n",	"lambda:",	misc.lambda	);
	fprintf(fp, "%-15s %15.5le\n",	"_qs_:",	misc._qs_	);
	fprintf(fp, "%-15s %15.5le\n",	"qs0_:",	misc.qs0_	);

	fprintf(fp, "\n");
	fprintf(fp, "%-15s %15d       # parameters.nch\n",
					"Nof tch_::",	params.nch	);
	fprintf(fp, "%-15s %15d       # nreach_\n",
					"Nof Add::",	misc.nreach_	);
	fprintf(fp, "%-15s %15d       # input.ntimestep\n",
					"Nof Qt_::",	input.ntimestep	);
	fprintf(fp, "%-15s %15d       # input.ntimestep\n",
					"Nof qt_::",	input.ntimestep	);
	fprintf(fp, "%-15s %15d       # input.ntimestep\n",
					"Nof qo_::",	input.ntimestep	);
	fprintf(fp, "%-15s %15d       # input.ntimestep\n",
					"Nof qs_::",	input.ntimestep	);
	fprintf(fp, "%-15s %15d       # input.ntimestep\n",
					"Nof qv_::",	input.ntimestep	);
	fprintf(fp, "%-15s %15d       # input.ntimestep\n",
					"Nof Sbar_::",	input.ntimestep	);
	if(params.infex){
		fprintf(fp, "%-15s %15d       # input.ntimestep\n",
					"Nof f_::",	input.ntimestep	);
		fprintf(fp, "%-15s %15d       # input.ntimestep\n",
					"Nof fex_::",	input.ntimestep	);
	}

	if(misc.timestep || misc.idxclass){
		GIVEN_TI("Nof qt_:::");
		GIVEN_TI("Nof qo_:::");
		GIVEN_TI("Nof qs_:::");
		GIVEN_TI("Nof qv_:::");
		GIVEN_TI("Nof Srz_:::");
		GIVEN_TI("Nof Suz_:::");
		GIVEN_TI("Nof S_:::");
		GIVEN_TI("Nof Ea_:::");
		GIVEN_TI("Nof ex_:::");
	}
	fprintf(fp, "\n");


	fprintf(fp, "%-15s\n", "tch_::");
	for(i=0; i<params.nch; i++)
		fprintf(fp, "%15.5le\n", misc.tch_[i]);

	fprintf(fp, "%-15s\n", "Add::");
	for(i=0; i<misc.nreach_; i++)
		fprintf(fp, "%15.5le\n", misc.Add[i]);


	if(misc.timestep || misc.idxclass){
		if(misc.timestep){
			st = misc.timestep - 1;
			et = misc.timestep;
		}else{
			st = 0;
			et = input.ntimestep;
		}

		if(misc.idxclass){
			si = misc.idxclass - 1;
			ei = misc.idxclass;
		}else{
			si = 0;
			ei = misc.nidxclass;
		}
	}

	if(flg.wide){
		fprintf(fp, "%-15s %-15s %-15s %-15s %-15s %-15s",
				"Qt_::", "qt_::", "qo_::", "qs_::", "qv_::",
				"Sbar_::");
		if(params.infex)
			fprintf(fp, " %-15s %-15s", "f_::", "fex_::");
		fprintf(fp, "\n");

		for(i=0; i<input.ntimestep; i++){
			fprintf(fp, "%15.5le %15.5le %15.5le %15.5le "
				    "%15.5le %15.5le",
					misc.Qt_[i],
					misc.qt_[i][misc.nidxclass],
					misc.qo_[i][misc.nidxclass],
					misc.qs_[i],
					misc.qv_[i][misc.nidxclass],
					misc.Sbar_[i]);
			if(params.infex)
				fprintf(fp, " %15.5le %15.5le",
						misc.f_[i], misc.fex_[i]);
			fprintf(fp, "\n");
		}

		if(misc.timestep || misc.idxclass){
			fprintf(fp, "Given ");
			if(misc.timestep)
				fprintf(fp, "timestep: %5d", misc.timestep);
			if(misc.timestep && misc.idxclass)
				fprintf(fp, ", ");
			if(misc.idxclass)
				fprintf(fp, "idxclass: %5d", misc.idxclass);
			fprintf(fp, "\n");

			fprintf(fp, "%-15s %-15s %-15s %-15s %-15s "
				    "%-15s %-15s %-15s %-15s\n", 
					"qt_:::", "qo_:::", "qs_:::", "qv_:::",
					"Srz_:::","Suz_:::","S_:::",
					"Ea_:::", "ex_:::");

			for(i=st; i<et; i++)
				for(j=si; j<ei; j++)
					fprintf(fp, "%15.5le %15.5le %15.5le "
						    "%15.5le %15.5le %15.5le "
						    "%15.5le %15.5le %15.5le\n",
						misc.qt_[i][j], misc.qo_[i][j],
						misc.qs_[i],	misc.qv_[i][j],
						misc.Srz_[i][j],misc.Suz_[i][j],
						misc.S_[i][j],	misc.Ea_[i][j],
						misc.ex_[i][j]);
		}
	}else{
		fprintf(fp, "%-15s %-15s %-15s %-15s %-15s\n",
				"Qt_::", "qt_::", "qo_::", "qs_::", "qv_::");
		for(i=0; i<input.ntimestep; i++)
			fprintf(fp, "%15.5le %15.5le %15.5le %15.5le %15.5le\n",
					misc.Qt_[i],
					misc.qt_[i][misc.nidxclass],
					misc.qo_[i][misc.nidxclass],
					misc.qs_[i],
					misc.qv_[i][misc.nidxclass]);

		fprintf(fp, "%-15s", "Sbar_::");
		if(params.infex)
			fprintf(fp, " %-15s %-15s", "f_::", "fex_::");
		fprintf(fp, "\n");

		for(i=0; i<input.ntimestep; i++){
			fprintf(fp, "%15.5le",
					misc.Sbar_[i]);
			if(params.infex)
				fprintf(fp, " %15.5le %15.5le",
						misc.f_[i], misc.fex_[i]);
			fprintf(fp, "\n");
		}

		if(misc.timestep || misc.idxclass){
			fprintf(fp, "Given ");
			if(misc.timestep)
				fprintf(fp, "timestep: %5d", misc.timestep);
			if(misc.timestep && misc.idxclass)
				fprintf(fp, ", ");
			if(misc.idxclass)
				fprintf(fp, "idxclass: %5d", misc.idxclass);
			fprintf(fp, "\n");

			fprintf(fp, "%-15s %-15s %-15s %-15s\n",
					"qt_:::", "qo_:::", "qs_:::", "qv_:::");
			for(i=st; i<et; i++)
				for(j=si; j<ei; j++)
					fprintf(fp, "%15.5le %15.5le %15.5le "
						    "%15.5le\n",
						misc.qt_[i][j], misc.qo_[i][j],
						misc.qs_[i],	misc.qv_[i][j]);

			fprintf(fp, "%-15s %-15s %-15s\n",
					"Srz_:::", "Suz_:::", "S_:::");
			for(i=st; i<et; i++)
				for(j=si; j<ei; j++)
					fprintf(fp, "%15.5le %15.5le %15.5le\n",
						misc.Srz_[i][j],misc.Suz_[i][j],
						misc.S_[i][j]);

			fprintf(fp, "%-15s %-15s\n",
					"Ea_:::", "ex_:::");
			for(i=st; i<et; i++)
				for(j=si; j<ei; j++)
					fprintf(fp, "%15.5le %15.5le\n",
						misc.Ea_[i][j], misc.ex_[i][j]);
		}
	}

	fclose(fp);


	return;
}

