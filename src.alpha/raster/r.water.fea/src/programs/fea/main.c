/*
** Copyright (C) 1993 by Nalneesh Gaur and Dr. Baxter Vieux
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice and this permission notice and disclaimer
** appear in all copies.  This software is provided "as is" without express 
** or implied warranty.  Modifications are permitted at own risk provided 
** that original developer is mentioned.
**
*/
# include "gis.h"
# include <math.h>

char timefile[64]=" ";
char manningsmap[100],sat_cond_map[100],cap_suc_map[100],porosity_map[100];
char slopemap[100];
char element[64] = "r.water.fea/";

short int **stiffness, *A;

int max,nsn;
int cbw,bw,noe;
int basin_no,no_stream,basinhydrograph =0,animate=0,infilmap=0;
int duration,mode=1,delta_t,out_node,monit_time;
int *nin,*nout,*sn,*stream_node;

double *easting, *northing, *le, *rhf, *capacitance, *manning;
double width,base,ch_length,nslope,rain_max,area;



int 
absolute(value)
int value;
{
	if(value < 0) return (-1*value);
	else return value;
}


/* Function to calculate the band width */
/* This program calculates the band width for a given matrix based on 
	nodein and nodeout */
void
calc_band_width(nin,nout,max,bw)
int *nin,*nout,max,*bw;
{
	int i,big;
	for(i=0;i<max-1;i++){
		big =(int) absolute(nin[i] - nout[i]);
		if(*bw < big)
			*bw = big;
	}
	(*bw) +=  1;
}

main(argc,argv)
int argc;
char *argv[];
{
	int i,j,signal;
	int bn, akchar;
	void timedata(),makestiffness_capacitance(),timecalc();
	void store_matrix(),find_row_no_and_element();

	struct{
		struct Flag *b;
		struct Flag *m;
	}flag;
	struct{
		struct Option *basin_no;
		struct Option *project;
		struct Option *slop;
		struct Option *mmode;
	}option;

	FILE *fp1;
	
	option.project = G_define_option();
	option.project->key = "project";
	option.project->type = TYPE_STRING;
	option.project->required = YES;
	option.project->gisprompt = "any,r.water.fea,project";
	option.project->description = "project name";
	
	
	option.slop = G_define_option();
	option.slop->key = "slope";
	option.slop->type = TYPE_STRING;
	option.slop->required = YES;
	option.slop->gisprompt = "old,raster,slope";
	option.slop->description = "slope raster map name";

	option.basin_no= G_define_option();
	option.basin_no->key = "basin_no";
	option.basin_no->type = TYPE_INTEGER;
	option.basin_no->required = YES;
	option.basin_no->description = "Basin number to be analysed.";

    	option.mmode = G_define_option();
	option.mmode->key = "mode";
    	option.mmode->type = TYPE_STRING;
	option.mmode->options = "1,2";
	option.mmode->required = NO;
	option.mmode->answer = "1";
	 option.mmode->description = "value of 2 for time varying, 1 default";

	flag.b=G_define_flag();
	flag.b->key = 'b';
	flag.b->description = "View basin level hydrographs.";

	flag.m=G_define_flag();
	flag.m->key = 'm';
	flag.m->description = "Draw animation maps at the basin level.";


	G_gisinit(argv[0]);
	if(G_parser(argc,argv))
		exit(-1);

	strcpy(slopemap,option.slop->answer);
	/* Initialization */
	basinhydrograph = 0;
	animate = 0;
	infilmap = 0;
	mode = 1;

	if(flag.b->answer == 1)
		basinhydrograph=1;
	if(flag.m->answer == 1)
		animate=1;
	if(strcmp(option.mmode->answer,"2") == 0)
		mode = 2;
	strcat(element,option.project->answer);
	bn = (int) atoi(option.basin_no->answer);

	fp1 = G_fopen_old(element,"input.file",G_mapset());
	if(fp1 == NULL){
		fprintf(stderr,"Fatal error - File opening\n");
		exit(1);
	}

	do{
		akchar=getc(fp1);
		if(akchar == '#'){
			fscanf(fp1,"%d",&basin_no);
			if (basin_no == bn)
				break;
		}
	}while(akchar != EOF);

	fscanf(fp1,"%lf",&area);
	fscanf(fp1,"%d",&max);
	easting = (double *) malloc(max * sizeof(double));
	northing =(double *)  malloc(max * sizeof(double));
	nin =      (int *) malloc(max * sizeof(int));
	nout =     (int *) malloc(max * sizeof(int));
	le =       (double *) malloc(max * sizeof(double));
	rhf =      (double *) malloc(max * sizeof(double));
	manning =  (double *) malloc(max * sizeof(double));
	if(easting == NULL||northing == NULL||nin==NULL||nout==NULL||le==NULL||rhf==NULL){
		fprintf(stderr,"INSUFFICIENT MEMORY \n");
		exit(2);
	}
	for(i=0;i<max;i++)
		fscanf(fp1,"%d %lf %lf",&akchar,&easting[i],&northing[i]);

	for(i=0;i<max-1;i++)
		fscanf(fp1,"%d %d %d",&akchar,&nin[i],&nout[i]);

	fscanf(fp1,"%d",&no_stream);

	stream_node = (int *) malloc(no_stream * sizeof(int));
	if(stream_node == NULL){
		fprintf(stderr,"INSUFFICIENT MEMORY - stream_node\n");
		exit(3);
	}

	for(i=0;i<no_stream;i++)
		fscanf(fp1,"%d",&stream_node[i]);

	fscanf(fp1,"%d",&nsn);
	sn =(int *)malloc(nsn * sizeof(int));
	if (sn ==NULL){
		fprintf(stderr,"INSUFFICIENT MEMORY - SN\n");
		exit (4);
	}

	for(i=0;i<nsn;i++)
		fscanf(fp1,"%d",&sn[i]);

	stiffness = (short int **) calloc(max , sizeof(short int *));
	if(stiffness == NULL){
		fprintf(stderr,"INSUFFICIENT MEMORY - STIFFNESS\n");
		exit(5);
	}

	for(i=0;i<max;i++){
		stiffness[i] =(short int *) calloc(max, sizeof(short int));
		if(!stiffness[i]){
			fprintf(stderr," INSUFFICIENT MEMORY - SUBROWS\n");
			exit(5);
		}
	}

	/* To find the outlet, There should be just one outlet */
	for(i=0;i<max-1;i++){
		signal = 0;
		for(j=0;j<max-1;j++)
			if(nout[i] == nin[j]){
				signal = 1; /* detected */
				break;
			}
		if(signal == 0){
			out_node = nout[i] -1;
			break;
		}
	}

	/* Calculating the band width */
	bw=0;
	calc_band_width(nin,nout,max,&bw);

	capacitance =(double *) calloc(max , sizeof(double));
	if(capacitance == NULL){
		fprintf(stderr,"INSUFFICIENT MEMORY - Capacitance \n");
	   	exit(6);
	}

 	timedata();

	makestiffness_capacitance();
     	/* printresult(); */

	if(max > 10 || bw > 3){
		noe = 0;
		find_row_no_and_element(&cbw,&noe,bw,max);

		A = (short int *) calloc (noe,sizeof(short int));
		if (A == NULL){
			fprintf(stderr,"Insufficient Memory\n");
			exit(1);
		}

		store_matrix(stiffness,A,max,cbw,bw);
		free(*stiffness);
		free(stiffness);
	}
	else
		fprintf(stderr,"Not using Banded method\n");


/* Free memory */	
	free(nin);
	free(nout);
	free(le);
	free(sn);
	fclose(fp1);

	timecalc();
 	return 0;
}
