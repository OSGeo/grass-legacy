#include "gis.h"
#include "fea.h"
#include <math.h>
#define PRIN 41
#define RAISE 0.66666666667 /*2/3*/
#define K_MF 10000000 /* ten million*/
#define SUWF_MF 1000
#define P_MF 1000

double *suwf,*phi,*degofs,*ksat,*infil,*Fvolume;
double *hold;

int t,b,l,r;
double diffx,diffy;
double hsize,vsize;

void
timecalc()
{
	char variable[180];
	char timeelement[64];
	int time,i,j,rainn_interval = 0,pr_time=0;
	int temp = 0 ;
	int ct=0;
	double rainn=0,raino = 0,*aold,*qold;
	double *qsum,*fstar,*caold;
	double hmr,velocity;

	double xarray[42],yarray[42];
	void get_cell_values();
	void const_value();
	void band_multiply();

	FILE *fp5,*fp6,*fp7;

	for(i=0;i<PRIN;i++){
		xarray[i] = 0.0;
		yarray[i] = 0.0;
	}

	aold =   (double *)malloc(max* sizeof(double));
	hold =   (double *)malloc(max * sizeof(double));
	qold =   (double *)malloc(max * sizeof(double));
	qsum =   (double *)malloc(max * sizeof(double));
	caold =  (double *)malloc(max * sizeof(double));
	fstar =  (double *)malloc(max * sizeof(double));
	infil =  (double *)malloc(max * sizeof(double));
	Fvolume =(double *)malloc(max * sizeof(double));
	ksat =   (double *)malloc(max * sizeof(double));
	suwf =   (double *)malloc(max * sizeof(double));
	phi =    (double *)malloc(max * sizeof(double));
	degofs = (double *)malloc(max * sizeof(double));

	if(ksat == NULL||suwf == NULL||phi == NULL||degofs == NULL||Fvolume == NULL||infil == NULL){
		fprintf(stderr,"Memory allocation error\n");
		exit(10);
	}

	if(aold == NULL||hold == NULL||qold == NULL||qsum == NULL||caold == NULL||fstar == NULL){
		fprintf(stderr,"Memory allocation error\n");
		exit(11);
	}

	if(mode == 1)
		rainn = rain_max;
	if(mode == 2){
		sprintf(timeelement,"%s/timefiles",element);
		fp5 = (FILE *)G_fopen_old(timeelement,timefile,G_mapset());
		if(fp5 == NULL){
			fprintf(stderr,"FILE OPENING ERROR - TIMEFILE\n");
			exit(13);
		}
	}
	if(infilmap == 1){
		get_cell_values(easting,northing,max,ksat,sat_cond_map);
		get_cell_values(easting,northing,max,suwf,cap_suc_map);
		get_cell_values(easting,northing,max,phi,porosity_map);
		for(i=0;i<max;i++){
			ksat[i] = ksat[i]/K_MF;
			suwf[i] = suwf[i]/SUWF_MF;
			phi[i] = phi[i]/P_MF;
		}
	}
	else{
		fprintf(stderr,"Please enter infiltration parameters:\n");
		strcpy(variable,"Saturated conductivity [m/s]");
		const_value(variable,max,ksat,0.000008,0.0);
		strcpy(variable,"Capillary suction at wetting front [m]");
		const_value(variable,max,suwf,2.00,0.00);
		strcpy(variable,"Porosity");
		const_value(variable,max,phi,0.9,0.0);
	}

	strcpy(variable,"Degree of saturation");
	const_value(variable,max,degofs,1.0,0.0);

	/* discharge.basin will aold the results of outlet node of the basin
	needed by the channel routing program*/
	fp6 = G_fopen_new(element,"discharge.basin");
	if (fp6 == NULL){
		fprintf(stderr,"Discharge file opening ERROR\n");
		exit(14);
	}

	/* map.disp is the map to be used by s.surf.idw needed to make a map */ 
	if(animate == 1){
		fp7 = G_fopen_append(element,"disch_file");
		if(fp7 == NULL){
			fprintf(stderr,"File opening error\n");
			exit(16);
		}
		fprintf(fp7,"Z\n%d\n",max);
	}

	temp = (int)(monit_time * 60/PRIN);
	if(delta_t > temp){
		delta_t = temp;
		fprintf(stderr,"[Time step set to  %d]\n",temp);
	}
	for(i=0;i<max;i++){
		Fvolume[i]= 0.0;
		infil[i]= 0.0;
		aold[i] = 0.0;
		hold[i] = 0.0;
		qold[i] = 0.0;
		qsum[i] = 0.0;
		caold[i] = 0.0;
		fstar[i] = 0.0;
	}


	fprintf(stderr,"\nStarting time step calculation ............");
	/* Start time step calculation, time itself is in seconds */
	for(time=0;time <= monit_time * 60; time +=delta_t){
		G_percent(time,monit_time * 60,1);
		/*update rain */
		if(mode == 2 && time >= rainn_interval*60){
			if(!feof(fp5)){
		       	fscanf(fp5,"%d %lf",&rainn_interval,&rainn);
	        	if(rainn < 0)
					rainn = -1*rainn;
			}
		}
		if(time <= duration*60)
			raino = rainn/360000.0; /* 360000 because cm/hr to m/sec */
		else
			raino = 0.0;
	/*Assign the last computed values to the old location */
		for(i=0;i<max;i++){
			caold[i] = 0.0;
		}
    /*calculation of Infiltration */

	infiltration();
	/* Multiplying global capacitance matrix with the old depth value */
	/* Capacitance matrix is lumped */
		for(i=0;i<max;i++)
			caold[i] += capacitance[i] * aold[i];
	/* solution begins here */
		/* Multiply global stiffness matrix (A vector) with qold[] i.e qnew[i] */
			for(i=0;i< max;i++)
				qsum[i] = 0.0;
 			
			if(max <= 10 || bw <= 3){

				for(i=0;i<max;i++)
					for(j=0;j<max;j++)
						qsum[j] += stiffness[j][i] * qold[i];
			} 
			else{
				/* carry out banded multiplication */
				band_multiply(A,qsum,qold,max,cbw,bw);
			}

		/* Compute global sum vector */
			for(i=0;i<max;i++){
				fstar[i] = caold[i] + delta_t * ((rhf[i] * (raino - infil[i])) - qsum[i]);
				if(fstar[i] < 0)
					fstar[i] = 0.0;
			}
		/* Solve the system of equations - LUMPED FORMULATION */
			for(i=0;i<max;i++){
         	   	   aold[i] = fstar[i]/capacitance[i]; 
			   hold[i] = aold[i]/width;
			   qold[i] = (pow((double)hold[i],(double)RAISE) * aold[i])/manning[i];
			}
			for(i=0;i<no_stream;i++){
				temp = stream_node[i] - 1;
 				hold[temp] =(-1 * base + sqrt((base * base + 4.0*nslope * aold[temp])))/(2.0*nslope);
				hmr = aold[temp]/(base + (2.0*hold[temp]*sqrt((double)(nslope*nslope + 1))));
				velocity = pow((double)hmr,(double)RAISE)/manning[i];
 				qold[temp] = velocity * aold[temp];
			}

	/* Solution process ends here */
	/* printing intermediate results */
		if(time >= pr_time){
			/* print results to map.disp to show a map */
			if(animate == 1){
				for(i=0;i<max;i++)		
					fprintf(fp7,"%lf ",hold[i]);
				fprintf(fp7,"\n");
			}
			fprintf(fp6,"%lf\n",qold[out_node]);
			pr_time += (int)((monit_time * 60)/PRIN);
			if(basinhydrograph == 1){
				xarray[ct] = time/(monit_time * 60.0);
				yarray[ct++] = qold[out_node];
			}
		}
  	}
	xarray[0] = 0.0;
	/* stop time step calcultion */
	printf("\n");
	if(basinhydrograph == 1){
		fprintf(stderr,"[Hit RETURN to view hydrograph]\n");
		gets(variable);
		system("d.erase");
		R_open_driver();
		make_setup();
		putxy(xarray,yarray,monit_time);
		sprintf(variable,"Hydrograph of basin #%d",basin_no);
		R_move_abs(l +(int)(0.35*diffx) , b -(int)(0.04*diffy));
		R_text(variable);
 		R_close_driver();
	}
}
