#include<stdio.h>
#include "define.h"
#include "global.h"

read_par()
{
    int index;

    FILE *stream;

    char string[80];

    printf("\n");
    printf("\n");
    printf("\n ***************************************************************");

    printf("\n Space units:");
    printf("\n 1 English (ft, Farenheight");
    printf("\n 2 Metric (cm, Celcius)");
    printf("\n ");
    scanf("%d",&units_space);

    if(units_space == ENGLISH) {
        printf("\n Is GRASS data in metric units (dem, cell size):");
        printf("\n 0 = No");
        printf("\n 1 = Yes");
        printf("\n ");
        scanf("%d",&convert_GIS_units);
    }
    else if(units_space == METRIC) {
        printf("\n Is GRASS data in metric units (dem, cell size):");
        printf("\n 0 = No");
        printf("\n 1 = Yes");
        printf("\n ");
        scanf("%d",&convert_GIS_units);
    }
    else {
        printf("\n ERROR: Space units not properly specified ");
        exit(0);
    }

    printf("\n Time units:");
    printf("\n 1 Seconds");
    printf("\n 2 Minutes");
    printf("\n ");
    scanf("%d",&units_time);
    if((units_time != SECONDS) && (units_time != MINUTES)) {
        printf("\n ERROR: Time units not properly specified ");
        exit(0);
    }

    printf("\n Enter the duration of the simulation ");
    if(units_time == SECONDS)
	printf("(in seconds):\n ");
    if(units_time == MINUTES)
	printf("(in minutes):\n ");
    scanf("%f",&tfin);

    printf("\n Enter the time increment ");
    if(units_time == SECONDS)
	printf("(in seconds):\n ");
    if(units_time == MINUTES)
	printf("(in minutes):\n ");
    scanf("%f",&dt);

    printf("\n Enter the adjustment factor for the Newton Ralphson ");
    printf("\n computations (0.8 suggested): ");
    printf("\n ");
    scanf("%f",&theta);

    printf("\n Enter the air temperature ");
    if(units_space == ENGLISH)
        printf("(degrees F):\n ");
    if(units_space == METRIC)
        printf("(degrees C):\n ");
    scanf("%f",&temp);

    printf("\n");
    printf("\n Enter the resistance law: ");
    printf("\n 1 Mannings's Law");
    printf("\n 2 Mannings's Law with laminar flow");
    printf("\n 3 Chezy's Law with laminar flow");
    printf("\n 4 Chezy's Law");
    printf("\n ");
    scanf("%d",&nres);
    if((nres < 1) || (nres > 4)) {
        printf("\n ERROR: Resistance law not properly specified ");
        exit(0);
    }

    printf("\n Erosion flag:");
    printf("\n 0 No erosion");
    printf("\n 1 Erosion");
    printf("\n ");
    scanf("%d",&erosion);
    if((erosion != EROSION) && (erosion != NO_EROSION)) {
        printf("\n ERROR: Resistance law not properly specified ");
        exit(0);
    }

    npart = 0;
    if(erosion == EROSION) {
        printf("\n Enter the number of sediment size classes (<= 7): ");
        scanf("%d",&npart); 
    }
/*
 *  Read in file with soil infiltration parameters for given textures.
 */
    if(units_space == ENGLISH)
	strcpy(string,"soil.english");
    else if(units_space == METRIC)
	strcpy(string,"soil.metric");
    else {
	printf("\n ERROR:  Space units not properly specified");
	exit(0);
    }

    stream = open_file(string,"r");

    max_soil = 1;
    min_soil = MAX_SOIL;

    while(fscanf(stream,"%d",&index) != EOF) {
        fscanf(stream,"%f",&porosity[index]);
	fscanf(stream,"%f",&Sint[index]);
	fscanf(stream,"%f",&Smax[index]);
	fscanf(stream,"%f",&fmin[index]);
	fscanf(stream,"%f",&G[index]);

	printf("\n");
	printf("%2d ",index);
	printf("%5.3f ",porosity[index]);
	printf("%5.3f ",Sint[index]);
	printf("%5.3f ",Smax[index]);
	printf("%6.3f ",fmin[index]);
	printf("%6.3f ",G[index]);
	fflush(stdout);
	fflush(stderr);

	if(max_soil < index)
	    max_soil = index;
	if(min_soil > index)
	    min_soil = index;

	if((min_soil >= MAX_SOIL) || (max_soil >= MAX_SOIL)) {
	    printf("\n ERROR:  Too many soil textures.  Increase MAX_SOIL in global.h");
 	    exit(0);
	}
    }
    printf("\n");
    printf("\n");
    printf("\n");
    fclose(stream);
}
