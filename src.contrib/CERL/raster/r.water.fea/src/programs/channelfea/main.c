#include <string.h>
#include <math.h>
#include <stdlib.h>
#include "gis.h"
#include "local_proto.h"

char element[64] ="r.water.fea/";
int 
compare (int *a, int *b)
{
	return *a - *b;
}

double 
meanslope (char *slopemap)
{
	char *mapset;
	CELL *slopebuff;
	int nrows,ncols;
	int row,col;
	int fdslope;
	long int count = 0,total = 0;
	double average;

	mapset = G_find_cell(slopemap,"");
	fdslope = G_open_cell_old(slopemap, mapset);

	slopebuff = G_allocate_cell_buf();

	nrows = G_window_rows();
	ncols = G_window_cols();
	for(row=0;row <nrows;row++){
		G_get_map_row(fdslope,slopebuff,row);
		for(col=0;col < ncols;col++)
			if(slopebuff[col] > 0){
				total += slopebuff[col];
				count++;
			}
	}
	G_close_cell(fdslope);
	average =(double) total/(count*100);
	return(average);
}

short int **stiffness;
int max,nsn,duration,monit_time,delta_t;
int *nin1,*nout1,*nin,*nout,*sn,*only_out;
double *rhf,*length, *capacitance,slope,*nslope,*base,*manning,wat_area,max_dis,rain_max;

int 
main (int argc, char *argv[])
{
	int i,j,k;
	int count,index;
	struct Option *project,*slop;
	void timedata(),makestiffness_capacitance(),timecalc();
	FILE *fp1; 
	
	G_gisinit(argv[0]);

	project = G_define_option();
	project->key = "project";
	project->type = TYPE_STRING;
	project->required = YES;
	project->gisprompt = "any,r.water.fea,project";
	project->description = "project name";

	slop = G_define_option();
	slop->key = "slope";
	slop->type = TYPE_STRING;
	slop->required = YES;
	slop->gisprompt = "old,cell,raster";
	slop->description = "slope raster map name";

	if(G_parser(argc,argv))
		exit(-1);
	strcat(element,project->answer);
	fp1 = G_fopen_old(element,"input.basin",G_mapset());
	if(fp1 ==NULL ){ 
		fprintf(stderr,"Fatal error - File opening\n");
		exit(1);
	}
	fscanf(fp1,"%d",&max);
	nin =  (int *)calloc(max , sizeof(int));
	nout = (int *)calloc(max , sizeof(int));
	length = (double *)calloc(max, sizeof(double));
	if(nin == NULL||nout == NULL||length==NULL){
		fprintf(stderr,"Insufficient Memory -  Integer\n");
		exit(2);
	}
	for(i=0;i<max-1;i++)
		fscanf(fp1,"%d %d ",&nin[i],&nout[i]);
	fscanf(fp1,"%d",&nsn);
	sn = (int *)malloc(nsn * sizeof(int));
	if (sn ==NULL){
		fprintf(stderr,"Insufficient memory - SN\n");
		exit(3);
	}
	index 		= max - nsn;
	nin1		=     (int *)calloc(index,sizeof(int));
	nout1		=    (int *)calloc(index,sizeof(int));
	only_out	=     (int *)calloc(max,sizeof(int));
	manning 	= (double *)calloc(index,sizeof(double));
	nslope 		=  (double *)calloc(index,sizeof(double));
	base 		=    (double *)calloc(index,sizeof(double));
	if(nin1 == NULL||nout1 == NULL||only_out == NULL||manning == NULL||nslope == NULL||base == NULL){
		fprintf(stderr,"Insufficient memory \n");
		exit(4);
	}
	stiffness=(short int **)malloc(index*sizeof(short int *));
	if(stiffness == NULL)
	   fprintf(stderr,"Insufficient Memory - STIFFNESS\n");
	for(i=0;i<index;i++){
		stiffness[i]=(short int *)malloc(index*sizeof(short int));
		if(!stiffness[i]){
			fprintf(stderr,"Insufficient Memory - SUBROWS\n");
			exit(5);
		}
	}
	capacitance = (double *)malloc(index * sizeof(double));
	if(capacitance == NULL){
	   fprintf(stderr,"Insufficient memory - CAPACITANCE\n");
	   exit(6);
	}
	rhf = (double *) calloc(index , sizeof(double));
	if(rhf == NULL)
	   fprintf(stderr,"Insufficient memory - RHF\n");
	for(i=0;i<nsn;i++)
		fscanf(fp1,"%d",&sn[i]);
	fscanf(fp1,"%lf",&wat_area);
	/* Filling only_out with unique values */
	for(i=0;i<max-1;i++)
		only_out[i] = nout[i];

	qsort(only_out,max-1,sizeof(int),compare);
	count = 0;
	only_out[count++] = only_out[0];

	for(i=0;i<max -2;i++){
		if(only_out[i] == only_out[i+1])
			continue;
		else
			only_out[count++] = only_out[i+1];
	}
		
	/* Filling up nin1 and nout1 */
	count = 0;
	for(i=0;i<max -1;i++)
		for(j=0;j<max-nsn;j++){
			if(nin[i] == only_out[j]){
				nin1[count] = j;
				for(k=0;k<max - nsn;k++)
					if(nout[i] == only_out[k]){
						nout1[count++] = k;			
						break; /* Do not check any further */
					}
			}
		} 
	timedata(); 
	makestiffness_capacitance();

	slope = (double) meanslope(slop->answer);
	if(slope <= 0)
		slope = 0.02; /* 2 percent slope */
	free(sn);
	fclose(fp1);
	timecalc();  
	return(0);
}
