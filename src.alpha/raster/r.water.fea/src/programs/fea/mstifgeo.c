/* Assembling the stiffness matrix, capacitance matrix and the
right hand force vector are done in here */

#include <stdio.h>
#include "fea.h"

static double *lenxwid,*widthel;

static void
righthandforce()
{
	int i,j,temp1=0;
	int signal;

	for(i=0;i<max;i++)
		rhf[i] = 0.0;

	for(i=0;i<max;i++){
		signal=0;
		temp1=nin[i]-1;
		for(j=0;j<no_stream;j++){
			if(nin[i] == stream_node[j]){
				lenxwid[i] = le[i]*base;
				widthel[i] = base;
				signal = 1;
				break;
			}	
		}
		if(signal == 0){
			lenxwid[i] = le[i]*width;
			widthel[i] = width;
		}
	}
	for(i=0;i<max-1;i++){
		if((out_node+1) == nout[i]){
			lenxwid[i] = le[i]*base;
			widthel[i] = base;
		}
	}
	/*Obtaining the righthand force vector*/
		for(i=0;i<max-1;i++){
			temp1 = nout[i]-1;
			rhf[temp1] += lenxwid[i];
		}
		for(i=0;i<nsn;i++){
			temp1 = sn[i] - 1;
			for(j=0;j<max-1;j++){
				if(sn[i] == nin[j])
					rhf[temp1] = 0.5*lenxwid[j];
			}
		}
}

int compare(a,b)
int *a,*b;
{
	return *a - *b;
}

void make_capacitance()
{
	int i,j,count,temp1;
	int *sortarray;
	struct out {
		int unout;
		double numerator;
		double denominator;
	} *uniq;
	
	sortarray = (int *) calloc((max-1),sizeof(int));
	if(sortarray == NULL){
		fprintf(stderr,"INSUFFICIENT MEMORY - sortarray\n");
		exit(7);
	}
	for(i=0;i<max-1;i++){
		sortarray[i] = nout[i];
	}

	qsort(sortarray,(max-1),sizeof(int),compare);
		
	count = 1;
	for(i=0;i<(max-2);i++){
		if(sortarray[i] != sortarray[i+1]){
			count++;
		}
	}

	uniq = ( struct out *)calloc(count,sizeof(struct out));
	if(uniq == NULL){
		fprintf(stderr,"INSUFFICIENT MEMORY - OUT\n");
		exit(7);
	}

	uniq[0].unout = sortarray[0];
	count = 0;
	for(i=0;i<(max-1);i++){
		if(uniq[count].unout != sortarray[i]){
			count++;
			uniq[count].unout = sortarray[i];
		}
	}
	count++;
	
	for(i=0;i<count;i++)
		for(j=0;j<(max-1);j++){
			if(uniq[i].unout == nout[j]){
				uniq[i].numerator += lenxwid[j];
				uniq[i].denominator += widthel[j];
			}
		}

	/* Assembling the Capacitance matrix */

	for(i=0;i< count;i++){
		temp1 = uniq[i].unout - 1;
		capacitance[temp1] = uniq[i].numerator/uniq[i].denominator;
	}
	for(i = 0;i < nsn; i++){
		temp1 = sn[i] - 1;
		capacitance[temp1] = le[i]/2.0;
	}
	free(sortarray);
}

void
makestiffness_capacitance()
{
	int temp1, temp2, i, j;

	fprintf(stderr,"\nASSEMBLING MATRICES .........");

	lenxwid = (double *)calloc(max,sizeof(double));
	widthel = (double *)calloc(max,sizeof(double));
	if( lenxwid == NULL||widthel == NULL ){
		fprintf(stderr,"INSUFFICIENT MEMORY - WIDTH\n");
		exit(9);
	}


	righthandforce();

	make_capacitance();

	for(i = 0;i< max ;i++){  
		G_percent(i,max,1);
		stiffness[i][i] = 1;
		for(j=0;j< max -1;j++){ /* j is element index */
			temp1 = nin[j] - 1;
			temp2 = nout[j] -1;
			if(i == temp2)
				stiffness[i][temp1] = -1;
		}
	}
	/* applying the boundary conditions */
	for (i = 0; i < nsn; i++){ 
		temp1 = sn[i] - 1;
		stiffness[temp1][temp1] = 1;
	}

	free(lenxwid);
	free(widthel);
	fprintf(stderr,"100%% done.\n");
}
