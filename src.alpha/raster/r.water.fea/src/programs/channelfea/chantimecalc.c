#include "gis.h"
#include <math.h>
#include <stdlib.h>
#include <string.h>
#define RAISE 2.0/3.0
#define PRIN 41
extern int max,nsn,duration,monit_time,delta_t,*only_out;
extern int *nin,*nout,*nin1;
extern short int **stiffness;
extern double *rhf,*capacitance,slope;
extern double *nslope,*base,*manning; 
extern char element[64];
char icon[25];
char *colors[7] = {"yellow","red","green","blue","magenta","orange","violet"};
double *lq;
/* function to make the righthand force vector */
void
righthandforce()
{
	int i,j,temp1;

	for(i=0;i<max - nsn;i++)
		rhf[i] = 0.0;

	/* Re_forming the right hand force vector */
	for(i=0;i<max-1;i++){
		temp1=nin[i];
		for(j=0;j<max-nsn;j++){
			if(nout[i] == only_out[j])
				rhf[j] += lq[temp1];
		}
	}
}

int t,b,l,r;
double diffx,diffy;
double hsize,vsize;

void
timecalc()
{
	double place;
	int time,i,j,pr_time=0;
	int ct=0;

	double *xarray,**yarray;
	double *aold,*qold;
	double *qsum,*fstar,*caold;
	double hmr,velocity,head;
	void righthandforce();
	FILE *fpjunc,*fpq;

	aold =(double *) malloc((max-nsn)* sizeof(double));
	qold =(double *) malloc((max-nsn)* sizeof(double));
	qsum =(double *) malloc((max-nsn)* sizeof(double));
	caold =(double *) malloc((max-nsn)* sizeof(double));
	fstar =(double *) malloc((max-nsn)* sizeof(double));
	lq = (double *) malloc(max * sizeof(double));
	if(aold == NULL||qold == NULL||qsum == NULL||caold == NULL||fstar == NULL||lq == NULL){
		fprintf(stderr,"Memory allocation error\n");
		exit(10);
	}
	for(i=0;i<max -nsn;i++){
		aold[i] = 0.0;
		qold[i] = 0.0;
		qsum[i] = 0.0;
		caold[i] = 0.0;
		fstar[i] = 0.0;
	}
	for(i=0;i<max;i++)
		lq[i] = 0.0;


	fpjunc = G_fopen_new(element,"disch.junction");
	if(fpjunc == NULL){
		fprintf(stderr,"File opening error\n");
		exit(14);
	}
	else{
		fprintf(fpjunc,"TIME(min)");
		for(i=0;i<max - nsn;i++)
			fprintf(fpjunc,"%10d ",only_out[i]);
		fprintf(fpjunc,"\n");
	}
	fpq = G_fopen_old(element,"disch.basin",G_mapset());
	if(fpq == NULL){
		fprintf(stderr,"File opening error\n");
		exit(14);
	}

	xarray = (double *) calloc((PRIN + 1), sizeof(double));
	if(xarray == NULL){
		fprintf(stderr,"Insufficient Memory - x\n");
		exit(1);
	}
	yarray = (double **) calloc((max-nsn), sizeof(double *));
	if(yarray == NULL){
		fprintf(stderr,"Insufficient Memory - y\n");
		exit(1);
	}
	for(i=0;i<(max-nsn);i++){
		yarray[i] = (double *) calloc((PRIN + 1),sizeof(double ));
		if(!yarray[i]){
			  fprintf(stderr,"Insufficient Memory - y2\n");
			  exit(1);
	    }
	}

	time = (int)(monit_time * 60.0/PRIN);
	if(delta_t > time){
		delta_t = time;
	}

	fprintf(stderr,"Starting time step calculations ..........");
		/* Start time step calculation */
		for(time=0;time <= monit_time*60;time += delta_t){
			G_percent(time,monit_time * 60,1);
			/* Assign the last computed values to the old location */
			for(i=0;i<max-nsn;i++){
				caold[i] = 0;
			}
		/* Multiplying global capacitance matrix with the old depth value, capacitance matrix is lumped */
			for(i=0;i<max -nsn;i++)
				caold[i] += capacitance[i] * aold[i];
		/* Multiply global stiffness matrix with qold[] i.e qnew[i] */
			for(i=0;i<max-nsn;i++)
				qsum[i] = 0.0;

			for(i=0;i<max -nsn;i++){
				for(j=0;j<max-nsn;j++)
					qsum[j] += stiffness[j][i] * qold[i];
			}
		/* Compute global sum vector */
			for(i=0;i<max -nsn;i++){
				fstar[i] = caold[i] +delta_t*(rhf[i] - qsum[i]);
			}
		/* Solve the system of equations - LUMPED FORMULATION */
			for(i=0;i<max -nsn;i++){
				aold[i] = fstar[i]/capacitance[i]; 
				/* calculate values on the righthandside */
 				head =(-1 * base[i] + sqrt((double)(base[i] * base[i] + 4*nslope[i] * aold[i])))/(2*nslope[i]);
				hmr = aold[i]/(base[i] + (2*head*sqrt((double)(nslope[i]*nslope[i] + 1))));
				velocity = (1/manning[i])*pow((double)hmr,(double)RAISE)*sqrt((double)slope);
 				qold[i] = velocity * aold[i];
			}
		/* Solution process ends here */
	/* printing intermediate results */
		if(time >= pr_time){
			for(i=1;i<max;i++)
				fscanf(fpq,"%lf",&lq[i]);
			righthandforce();
			for(i=0;i<max-nsn-1;i++)
				for(j=0;j<max-nsn;j++){
					if(nin1[i] == only_out[j])
						qold[j] = rhf[j];
				}
			if((max - nsn) == 1)
				qold[0] = rhf[0];
			/* print discharge to file */
				fprintf(fpjunc,"%-5d ",time/60);
				for(i=0;i<max-nsn;i++){
					fprintf(fpjunc,"%8.2lf   ",qold[i]);
					yarray[i][ct] = qold[i];
				}
				fprintf(fpjunc,"\n");
			pr_time += (int)((monit_time * 60)/PRIN );
		    xarray[ct] = time/(monit_time * 60.0);	
			ct++;
		}
  	}
	/* stop time step calcultion */
	 system("d.erase");
	 R_open_driver();
	 make_setup();
	 /* identification marks for hydrographs */
	 for(i=0;i<max-nsn;i++){
		place = i*0.1 + .15;
		R_standard_color(D_translate_color(colors[i]));
		R_move_abs(l +(int)(place*diffx) , b -(int)(0.07*diffy));
		R_cont_abs(l +(int)((place + 0.07)*diffx) , b -(int)(0.05*diffy));
		R_move_abs(l +(int)((place + 0.03)*diffx) , b -(int)(0.02*diffy));
		sprintf(icon,"%3d",only_out[i]);
		R_text(icon);
	 }
	 putxy(xarray,yarray,monit_time,max-nsn);
	 R_close_driver();
} 
