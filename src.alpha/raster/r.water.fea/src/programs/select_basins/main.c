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
#include "gis.h"
char streammap[300];
char accum_map[300];
char aspectmap[300];
char streamanalysismap[200] = "fea.stream.";
char basinanalysismap[200] = "fea.basin.";
char element[64] = "r.water.fea/";

int no_of_basins;
main(argc,argv)
int argc;
char **argv;
{
	char command[400];
	G_gisinit(argv[0]);
	if(argc != 5){
		fprintf(stderr,"USAGE: %s project streammap accumulation_map aspectmap\n",argv[0]);
		exit(1);
	}

	strcat(element,argv[1]);
	strcat(streamanalysismap,argv[1]);
	strcat(basinanalysismap,argv[1]);

	strcpy(streammap,argv[2]);
	strcpy(accum_map,argv[3]);
	strcpy(aspectmap,argv[4]);
	sprintf(command,"g.remove rast=%s,%s > /dev/null",basinanalysismap,streamanalysismap);
	system(command);
	no_of_basins=0;
	do{
		sprintf(command,"d.frame -s r.water.fea1");
		system(command);
	}while(go_select_basins(no_of_basins));
	basinlink(no_of_basins,accum_map,aspectmap);
	sprintf(command,"g.remove rast=T_E_M_P_M_A_P,T_E_M_P_M_A_P2 > /dev/null");
	system(command);
	return 0;
}
