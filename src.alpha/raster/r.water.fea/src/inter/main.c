
/*
** Copyright (C) 1993 by Nalneesh Gaur and Dr. Baxter Vieux
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice and this permission notice and disclaimer
** appear in all copies.  This software is provided "as is" without express 
** or implied warranty.  Modifications are permitted at own risk provided 
** that original developer is mentioned.
*/
/* r.fea program, This program calls other programs */
#include "gis.h"
char basin[250],stream[250],accumulation[250],drain[250],slope[250],mmode[10] ;
char fflag[5],mflag[5],element[64] = "r.water.fea/";
char project[64];
void write_stop_at();
void startup();
void status();
int stop = 0; /* 0 - starting from begining */
int show_second = 0;
int prev_choice = 2;
main(argc,argv)
int argc;
char *argv[];
{
    	char stat[300];
	char *mapset;
	char proname[75];
	char command[400];
	char line[150];
	int value;
	int ret;
	FILE *fsa;


	G_gisinit(argv[0]);
	if(R_open_driver() != 0){
		R_close_driver();
		exit(-4);
	}
	else{
		R_close_driver();
	}

	while(1){
		if((mapset = G_ask_any("Enter project name",proname,"r.water.fea","",0)) == NULL){ 
		exit(3);
		}
		if(G_legal_filename(proname) != 1){
			fprintf(stderr,"**Project <%s> name illegal**\n",proname);
			exit(4);
		}
		if(G_find_file("r.water.fea",proname,G_mapset()))
			break;
		sprintf(stat,"\nProject name <%s> does not exist,\nShall a new one be created?",proname);
		if(G_yes(stat,1))
			break;
	}
	strcat(element,proname);
	strcpy(project,proname);
	

	if(G_find_file("r.water.fea",proname,G_mapset())== NULL){
		/* start afresh */

		mapset = G_ask_cell_old("Enter the name of the [BASIN] map",basin);
		if(mapset == NULL)
			exit(1);
		sprintf(basin,"%s",G_fully_qualified_name(basin,mapset));
		mapset = G_ask_cell_old("Enter the name of the [STREAM] map",stream);
		if(mapset == NULL)
			exit(1);
		sprintf(stream,"%s",G_fully_qualified_name(stream,mapset));
		mapset = G_ask_cell_old("Enter the name of the [DRAINAGE] map",drain);
		if(mapset == NULL)
			exit(1);
		sprintf(drain,"%s",G_fully_qualified_name(drain,mapset));
		mapset = G_ask_cell_old("Enter the name of the [ACCUMMULATION] map",accumulation);
		if(mapset == NULL)
			exit(1);
		sprintf(accumulation,"%s",G_fully_qualified_name(accumulation,mapset));
		mapset = G_ask_cell_old("Enter the name of the [SLOPE] map",slope);
		if(mapset == NULL)
			exit(1);
		sprintf(slope,"%s",G_fully_qualified_name(slope,mapset));

		if(!G_yes("\n\nRainfall mode:\n\t1. spatially uniform and constant in time\n\t2. spatially uniform but varying in time\nShall the rainfall mode be set to 2",0))
			strcpy(mmode,"1");
		else
			strcpy(mmode,"2");

		if(!G_yes("\n\nDo you want to view basin level hydrographs ",0))
			strcpy(fflag," ");
		else
			strcpy(fflag,"-b");

		if(!G_yes("\n\nDo you wish to view animation maps for individual basins",0))
			strcpy(mflag," ");
		else
			strcpy(mflag,"-m");

		G__make_mapset_element(element);	
		write_stop_at(1);
		sprintf(command,"g.region align=%s",drain);
		system(command);

		status(1,stat);
		do{
			ret = usermenu(1,stat);
		}while(ret != 0);
	}
	else{
		fsa = (FILE *)G_fopen_old(element,"control",G_mapset());
		if(fsa == NULL){
			fprintf(stderr,"Program control file has been deleted.\n");
			exit(-1);
		}	
		stop = 1;
		fgets(line,150,fsa);
		sscanf(line,"%d",&value);
		fgets(line,150,fsa);
		sscanf(line,"%*s %s",basin);
		fgets(line,150,fsa);
		sscanf(line,"%*s %s",stream);
		fgets(line,150,fsa);
		sscanf(line,"%*s %s",drain);
		fgets(line,150,fsa);
		sscanf(line,"%*s %s",accumulation);
		fgets(line,150,fsa);
		sscanf(line,"%*s %s",slope);
		fgets(line,150,fsa);
		sscanf(line,"%*s %s",mmode);
		fgets(line,150,fsa);
		sscanf(line,"%*s %s",fflag);
		fgets(line,150,fsa);
		sscanf(line,"%*s %s",mflag);

		sprintf(command,"g.region align=%s",drain);
		system(command);

		if(value  != 1)
			startup(value + 1);

		status(value,stat);
		prev_choice = value;
		do{ 
			ret = usermenu(value,stat);
		}while(ret != 0);
	}	
	return(0);
}
