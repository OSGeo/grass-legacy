/* Program to input values for the time dependent calculation - basinFEA */
#include "gis.h"
extern int duration,monit_time,delta_t,*only_out;
extern double wat_area,rain_max;
extern double *nslope,*base,*length,*manning;
extern double *capacitance,*rhf;
extern short int **stiffness;		
extern int *nin1,*nout1,*sn,max,nsn;
extern char element[64];
extern int compare();
void
timedata()
{
	int akchar,icono,i,bn = 0;
	int infilmap;
	char junk[50];
	char timefile[64];
	FILE *fptd; /*fptd =>> file pointer to timedata */
	fptd = (FILE *)G_fopen_old(element,"timedata",G_mapset());
	if(fptd == NULL){
		fprintf(stderr,"timedata cannot be opened.\n");
		exit(8);
	}

	fscanf(fptd,"%*s%d",&duration);
	fscanf(fptd,"%*s%lf",&rain_max);
	fscanf(fptd,"%*s%d",&delta_t);
	fscanf(fptd,"%*s%d",&monit_time);
	fscanf(fptd,"%*s%s",timefile);
	fscanf(fptd,"%*s%d",&infilmap);
	if(infilmap == 1){
		fgets(junk,50,fptd);
		fgets(junk,50,fptd);
		fgets(junk,50,fptd);
		fgets(junk,50,fptd);
		fgets(junk,50,fptd);
	}
	/* Obtaining the stream channel properties */
			while ((akchar = getc(fptd)) != EOF)
				if(akchar == 'C'){
					fscanf(fptd,"%d",&bn);
					for(i=1;i<(max - nsn);i++){
						if(bn == only_out[i]){
					  		fscanf(fptd,"%lf",&length[i]);
							fscanf(fptd,"%lf",&nslope[i]);
							fscanf(fptd,"%lf",&base[i]);
							fscanf(fptd,"%lf",&manning[i]);
						}
					 }
				}
	/* manning,nslope and base at the 0th index don't have a value */
	/* set to defualt values of the same as outlet basin */
		for(i=0;i< max - nsn -1;i++)
			if(nout1[i] == 0){
				icono = nin1[i];
				base[0] = base[icono];
				nslope[0] = nslope[icono];
				manning[0] = manning[icono];
				break;
			}
	/* MODE 3 requires file - to be dealt in the next version */
	fclose(fptd);
}

void 
make_capacitance()
{
	int i,j,temp1,signal;
	int *valency;
	double *numerator;
	
	valency = (int *)calloc((max-nsn),sizeof(int));
	numerator = (double *)calloc((max-nsn),sizeof(double));
	if(valency == NULL||numerator == NULL){
		fprintf(stderr,"INSUFFICIENT MEMORY\n");
		exit(6);
	}
	for(i=0;i<(max-nsn);i++)
		valency[i] = 0;
        for(i=0;i<(max-nsn);i++){
		for(j=0;j<(max-nsn-1);j++){
			temp1 = nin1[j];
			if(i == nout1[j]){
				valency[i] += 1;
				numerator[i] += length[temp1];
			}
		}
		if(valency[i] == 0)
			valency[i] = 1;
	}
	
	/* Assembling the capacitance matrix */
	for(i=0;i< (max-nsn);i++){
		capacitance[i] = numerator[i]/valency[i];
	}
	for(i=0;i< (max-nsn);i++){
		temp1= nin1[i];
		signal = 0;
		for(j=0;j<(max-nsn);j++){
			if(nin1[i] == nout1[j]) {
				signal = 1;
				break;
			}
		}
		if(signal == 0){
			capacitance[temp1] = length[temp1];
		}
	}
}
	
void
makestiffness_capacitance()
{
	int temp1, i, j;
	for(i=0;i<max -nsn;i++){
		capacitance[i] = 0.0;
		for(j=0;j<max -nsn;j++)
			stiffness[i][j] = 0;
	}
	make_capacitance();
	for(i = 0;i< max -nsn;i++){  
		stiffness[i][i]=1;
		for(j=0;j<max -nsn -1;j++){
			temp1 = nin1[j];
			if(i == nout1[j])
				stiffness[i][temp1] = -1;
		}
	}
}
