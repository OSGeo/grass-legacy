#include "local_proto.h"


#if !defined(AVG) || (AVG != NONE && AVG !=BKWD && AVG != FRWD)
#	warning "AVG not defined or misdefined.  AVG NONE used."
#	define	AVG	NONE
#endif


double
get_lambda(void)
{
	int	i;
	double	retval;


	retval = 0.0;

#if	AVG == NONE
	for(i=0; i<misc.nidxclass; i++)
		retval += idxstats.Aatb_r[i] * idxstats.atb[i];
#elif	AVG == BKWD
	for(i=1; i<misc.nidxclass; i++)
		retval += idxstats.Aatb_r[i] * 
				(idxstats.atb[i] + idxstats.atb[i - 1]) / 2.0;
#elif	AVG == FRWD
	for(i=0; i<misc.nidxclass-1; i++)
		retval += idxstats.Aatb_r[i] * 
				(idxstats.atb[i] + idxstats.atb[i + 1]) / 2.0;
#endif


	return(retval);
}


void
initialize(void)
{
	int	i, j, t;
	double	A1, A2;

	
	misc.lambda = get_lambda();
	misc.lnTe_  = params.lnTe  + log(input.dt);
	misc.vch_   = params.vch   * input.dt;
	misc.vr_    = params.vr    * input.dt;
	misc.qs0_   = params.qs0   * input.dt;
	misc._qs_   = exp(misc.lnTe_ - misc.lambda);

	misc.tch_    = (double *) malloc(params.nch * sizeof(double));
	misc.tch_[0] = params.d[0] / misc.vch_;
	for(i=1; i<params.nch; i++)
		misc.tch_[i] = misc.tch_[0] +
				(params.d[i] - params.d[0]) / misc.vr_;

	misc.nreach_ = (int) misc.tch_[params.nch - 1];
	if((double) misc.nreach_ < misc.tch_[params.nch - 1])
		misc.nreach_++;
	misc.ndelay_ = (int) misc.tch_[0];

	misc.nreach_ -= misc.ndelay_;

	misc.Add     = (double *) malloc(misc.nreach_ * sizeof(double));
	for(i=0; i<misc.nreach_; i++){
		t = misc.ndelay_ + i + 1;
		if(t > misc.tch_[params.nch - 1]){
			misc.Add[i] = 1.0;
		}else{
			for(j=1; j<params.nch; j++){
				if(t <= misc.tch_[j]){
					misc.Add[i] = params.Ad_r[j - 1] +
					 (params.Ad_r[j] - params.Ad_r[j - 1]) *
					 (t - misc.tch_[j - 1]) /
					 (misc.tch_[j] - misc.tch_[j - 1]);
					break;
				}
			}
		}
	}


	A1 = misc.Add[0];
	misc.Add[0] *= params.A;
	for(i=1; i<misc.nreach_; i++){
		A2 = misc.Add[i];
		misc.Add[i] = A2 - A1;
		A1 = A2;
		misc.Add[i] *= params.A;
	}

	misc.Srz_ = (double **) malloc(input.ntimestep * sizeof(double *));
	misc.Suz_ = (double **) malloc(input.ntimestep * sizeof(double *));
	for(i=0; i<input.ntimestep; i++){
		misc.Srz_[i] = (double *) malloc(misc.nidxclass *
							sizeof(double));
		misc.Suz_[i] = (double *) malloc(misc.nidxclass *
							sizeof(double));
	}

	for(i=0; i<misc.nidxclass; i++){
		misc.Srz_[0][i] = params.Sr0;
		misc.Suz_[0][i] = 0.0;
	}

	misc.Sbar_    = (double *) malloc(input.ntimestep * sizeof(double));
	misc.Sbar_[0] = - params.m * log(misc.qs0_ / misc._qs_);

	misc.Qt_  = (double *) malloc(input.ntimestep * sizeof(double));
	for(i=0; i<input.ntimestep; i++)
		misc.Qt_[i] = 0.0;

	for(i=0; i<misc.ndelay_; i++)
		misc.Qt_[i] = misc.qs0_ * params.A;

	A1 = 0.0;
	for(i=0; i<misc.nreach_; i++){
		A1 += misc.Add[i];
		misc.Qt_[misc.ndelay_ + i] = misc.qs0_ * (params.A - A1);
	}


	return;
}


void
implement(void)
{
	int	i, j, k;
	double	Aatb_r;
	/* '_' prefix means precalculated one */
	double	R_;
	double	_qo_, _qv_;

	
	misc.S_   = (double **) malloc(input.ntimestep * sizeof(double *));
	misc.Ea_  = (double **) malloc(input.ntimestep * sizeof(double *));
	misc.ex_  = (double **) malloc(input.ntimestep * sizeof(double *));

	misc.qt_  = (double **) malloc(input.ntimestep * sizeof(double *));
	misc.qo_  = (double **) malloc(input.ntimestep * sizeof(double *));
	misc.qv_  = (double **) malloc(input.ntimestep * sizeof(double *));

	misc.qs_  = (double *) malloc(input.ntimestep * sizeof(double));
	misc.f_   = (double *) malloc(input.ntimestep * sizeof(double));
	misc.fex_ = (double *) malloc(input.ntimestep * sizeof(double));

	for(i=0; i<input.ntimestep; i++){
		misc.S_[i]  = (double *) malloc(misc.nidxclass * 
							sizeof(double));
		misc.Ea_[i] = (double *) malloc(misc.nidxclass * 
					       		sizeof(double));
		misc.ex_[i] = (double *) malloc(misc.nidxclass * 
					       		sizeof(double));

		misc.qt_[i] = (double *) malloc((misc.nidxclass + 1) * 
							sizeof(double));
		misc.qo_[i] = (double *) malloc((misc.nidxclass + 1) * 
					       		sizeof(double));
		misc.qv_[i] = (double *) malloc((misc.nidxclass + 1) * 
					       		sizeof(double));

		misc.qt_[i][misc.nidxclass] = 0.0;
		misc.qo_[i][misc.nidxclass] = 0.0;
		misc.qv_[i][misc.nidxclass] = 0.0;
		misc.qs_[i] = 0.0;

		if(params.infex && input.R_[i] > 0.0){
			misc.f_[i]   = input.dt *
				       get_f((i + 1) * input.dt,
						input.R_[i] / input.dt);
			misc.fex_[i] = input.R_[i] - misc.f_[i];
			R_ = misc.f_[i];
		}else{
			misc.f_[i]   = 0.0;
			misc.fex_[i] = 0.0;
			R_ = input.R_[i];
		}

		if(i){
			for(j=0; j<misc.nidxclass; j++){
				misc.Srz_[i][j] = misc.Srz_[i-1][j];
				misc.Suz_[i][j] = misc.Suz_[i-1][j];
			}
		}

		misc.qs_[i] = misc._qs_ * exp(- misc.Sbar_[i] / params.m);

		for(j=0; j<misc.nidxclass; j++){
#if	AVG == NONE
			Aatb_r = idxstats.Aatb_r[j];
#elif	AVG == BKWD
			Aatb_r = (idxstats.Aatb_r[j] + 
					(j > 0 ?
					 	idxstats.Aatb_r[j - 1]
						: 0.0)) / 2.0;
#elif	AVG == FRWD
			Aatb_r = (idxstats.Aatb_r[j] + 
					(j < misc.nidxclass - 1 ?
					 	idxstats.Aatb_r[j + 1]
						: 0.0)) / 2.0;
#endif

			misc.S_[i][j] = misc.Sbar_[i] +
				params.m * (misc.lambda - idxstats.atb[j]);
			if(misc.S_[i][j] < 0.0)
				misc.S_[i][j] = 0.0;

			misc.Srz_[i][j] -= R_;

			if(misc.Srz_[i][j] < 0.0){
				misc.Suz_[i][j] -= misc.Srz_[i][j];
				misc.Srz_[i][j] = 0.0;
			}

			misc.ex_[i][j] = 0.0;
			if(misc.Suz_[i][j] > misc.S_[i][j]){
				misc.ex_[i][j] = misc.Suz_[i][j] - 
							misc.S_[i][j];
				misc.Suz_[i][j] = misc.S_[i][j];
			}

			_qv_ = 0.0;
			if(misc.S_[i][j] > 0.0){
				_qv_ = (params.td > 0.0 ?
					misc.Suz_[i][j] / 
					(misc.S_[i][j] * params.td) * input.dt
					: - params.td * params.K0 *
					    exp(- misc.S_[i][j] / params.m));
				if(_qv_ > misc.Suz_[i][j])
					_qv_ = misc.Suz_[i][j];
				misc.Suz_[i][j] -= _qv_;
				if(misc.Suz_[i][j] < ZERO)
					misc.Suz_[i][j] = 0.0;
			}
			misc.qv_[i][j] = _qv_ * Aatb_r;
			misc.qv_[i][misc.nidxclass] += misc.qv_[i][j];

			misc.Ea_[i][j] = 0.0;
			if(input.Ep_[i] > 0.0){
				misc.Ea_[i][j] = input.Ep_[i] * 
					(1 - misc.Srz_[i][j] / params.Srmax);
				if(misc.Ea_[i][j] > params.Srmax - 
						   misc.Srz_[i][j])
					misc.Ea_[i][j] =
						params.Srmax - 
						misc.Srz_[i][j];
			}
			misc.Srz_[i][j] += misc.Ea_[i][j];

#if	AVG == NONE
			_qo_ = misc.ex_[i][j];
#elif	AVG == BKWD
			_qo_ = (j > 0 ?
					(misc.ex_[i][j] + misc.ex_[i][j - 1]) /
									2.0
					: 0.0);
#elif	AVG == FRWD
			_qo_ = (j < misc.nidxclass - 1 ?
					(misc.ex_[i][j] + misc.ex_[i][j + 1]) /
									2.0
					: 0.0);
#endif
			misc.qo_[i][j] = _qo_ * Aatb_r;
			misc.qo_[i][misc.nidxclass] += misc.qo_[i][j];

			misc.qt_[i][j] = misc.qo_[i][j] + misc.qs_[i];
		}
		misc.qo_[i][misc.nidxclass] += misc.fex_[i];
		misc.qt_[i][misc.nidxclass] = 
				misc.qo_[i][misc.nidxclass] + misc.qs_[i];

		misc.Sbar_[i] = misc.Sbar_[i] + 
				misc.qs_[i] - misc.qv_[i][misc.nidxclass];

		if(i + 1 < input.ntimestep)
			misc.Sbar_[i + 1] = misc.Sbar_[i];

		for(j=0; j<misc.nreach_; j++){
			k = i + j + misc.ndelay_;
			if(k > input.ntimestep - 1)
				break;
			misc.Qt_[k] += misc.qt_[i][misc.nidxclass] * 
								misc.Add[j];
		}
	}


	return;
}


/* Object function for hydrograph suggested by Servet and Dezetter(1991) */
double
get_Em(void)
{
	int	i;
	double	Em, numerator, denominator;


	misc.Qobs_bar_ = 0.0;
	numerator = 0.0;
	for(i=0; i<input.ntimestep; i++){
		misc.Qobs_bar_ += misc.Qobs_[i];
		numerator += pow(misc.Qobs_[i] - misc.Qt_[i], 2.0);
	}
	misc.Qobs_bar_ /= input.ntimestep;

	denominator = 0.0;
	for(i=0; i<input.ntimestep; i++)
		denominator += pow(misc.Qobs_[i] - misc.Qobs_bar_, 2.0);

	if(denominator == 0.0){
		fprintf(stderr, "\n** Em can not be resolved due to constant "
				"observed Q **\n");
		G_set_d_null_value(&Em, 1);
	}else{
		Em = 1.0 - numerator / denominator;
	}


	return(Em);
}


void
others(void)
{
	int	i;


	misc.Qt_bar_ = 0.0;
	for(i=0; i<input.ntimestep; i++){
		misc.Qt_bar_ += misc.Qt_[i];
		if(!i || misc.Qt_peak_ < misc.Qt_[i]){
			misc.Qt_peak_ = misc.Qt_[i];
			misc.tt_peak_ = i + 1;
		}
	}
	misc.Qt_bar_ /= input.ntimestep;

	if(file.Qobs){
		misc.Em = get_Em();
		for(i=0; i<input.ntimestep; i++){
			if(!i || misc.Qobs_peak_ < misc.Qobs_[i]){
				misc.Qobs_peak_ = misc.Qobs_[i];
				misc.tobs_peak_ = i + 1;
			}
		}
	}


	return;
}


void
topmodel(void)
{
	initialize();
	implement();
	others();


	return;
}

