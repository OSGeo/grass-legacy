
	/*---------------------------------------------------------*
	 *               AGNPS/GRASS Interface Project             *
	 *  Developed in the Agriculture Engineering Department    *
	 *                at Purdue University                     *
	 *                        by                               *
	 *         Raghavan Srinivasan and Bernard Engel           *
	 *                                                         *
	 *   (c)Copyright, 1992 Purdue Research Foundation, West   *
	 *   Lafayette, Indiana 47907. All Rights Reserved. Unless *
	 *   permission is granted, this material shall not be     *
	 *   copied, reproduced or coded for reproduction by any   *
	 *   electrical, mechanical or chemical processes,  or     *
	 *   combinations thereof, now known or later developed.   *
	 *---------------------------------------------------------*/

/*      June, 1991  Agricultural Engineering, Purdue University
	Raghavan Srinivasan (srin@ecn.purdue.edu)
		
	main(argc,argv)
		 
	argv should have the name of the AGNPS output file
	
	To print the watershed summary at the outlet of the watershed
	The output goes to aa3 printer
*/

#include <stdio.h>

main(argc,argv)
int argc;
char *argv[];

{

	int i, j;
	float temp_var;
	char buf[1024], title[40], *file_name[40];
	FILE *fs, *fd, *fopen(), *fclose();

	/*
	fprintf (stderr,"\nEnter the .nps file name to prepare the report --> ");
	scanf("%s",file_name);
	*/

	if ((fs = fopen(argv[1],"r")) == NULL){
		fprintf (stderr,"File not found\n");
		exit(0);
		}

	fd = fopen("temp.nps","w");

	fgets(buf,1024,fs);
	fgets(buf,1024,fs);
	fgets(buf,1024,fs);

	fprintf(fd,"\t\t\t Watershed Summary\n\n");
	fprintf(fd," Watershed Studied\t\t");

	fgets(buf,1024,fs);
	fprintf(fd,"%s ",buf);
	
	fprintf(fd,"\n");
	fprintf(fd,"The area of the watershed is ");
	fscanf(fs,"%s",title);
	i = atoi(title);
	fprintf(fd,"%35d acres\n",i);
	fprintf(fd,"The area of each cell is ");
	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%39.2f acres\n",temp_var);
	fprintf(fd,"The characteristic storm precipitation is ");
	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%22.2f inches\n",temp_var);
	fprintf(fd,"The strom energy-intensity value is ");
	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%28.0f\n",temp_var);
	fprintf(fd,"\n\t\t Values at the Watershed Outlet\n");
	fprintf(fd,"Cell number");
	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%53.0f",temp_var);
	fscanf(fs,"%d",&j);
	fprintf(fd," %d00\n",j);
	fprintf(fd,"Runoff Volume");
	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%51.1f inches\n",temp_var);
	fprintf(fd,"Peak runoff rate");
	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%48.0f cfs\n",temp_var);
	fprintf(fd,"Total Nitrogen in sediment");
	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%38.2f lbs/acre\n",temp_var);
	fprintf(fd,"Total soluble Nitrogen in runoff");
	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%32.2f lbs/acre\n",temp_var);
	fprintf(fd,"Soluble Nitrogen concentration in runoff");
	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%24.2f ppm\n",temp_var);
	fprintf(fd,"Total Phosphorus in sediment");
	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%36.2f lbs/acre\n",temp_var);
	fprintf(fd,"Total soluble Phosphorus in runoff");
	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%30.2f lbs/acre\n",temp_var);
	fprintf(fd,"Soluble Phosphorus concentration in runoff");
	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%22.2f ppm\n",temp_var);
	fprintf(fd,"Total soluble chemical oxygen demand");
	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%28.2f lbs/acre\n",temp_var);
	fprintf(fd,"Soluble chemical oxygen demand concentration in runoff");
	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%10.0f ppm\n\n\n",temp_var);

	fprintf(fd,"\t\t\t Sediment Analysis\n\n");
	fprintf(fd,"\t     Area Weighted                                     Area\n");
	fprintf(fd,"\t       Erosion     Delivery  Enrichment    Mean       Weighted\n");
	fprintf(fd,"Particle   Upland   Channel  Ratio    Ratio    Concentration   Yield    Yield\n");
	fprintf(fd,"  Type     (t/a)    (t/a)     (%%)                 (ppm)         (t/a)   (tons)\n");
	fprintf(fd,"______________________________________________________________________________\n");
	fprintf(fd,"CLAY");

	fscanf(fs,"%s",title);
	fscanf(fs,"%s",title);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%11.2f",temp_var);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%9.2f",temp_var);

	fscanf(fs,"%d",&i);
	fprintf(fd,"%8d",i);

	fscanf(fs,"%d",&i);
	fprintf(fd,"%10d",i);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%14.2f",temp_var);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%12.2f",temp_var);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%10.2f\n",temp_var);

	fprintf(fd,"SILT");
	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%11.2f",temp_var);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%9.2f",temp_var);

	fscanf(fs,"%d",&i);
	fprintf(fd,"%8d",i);

	fscanf(fs,"%d",&i);
	fprintf(fd,"%10d",i);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%14.2f",temp_var);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%12.2f",temp_var);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%10.2f\n",temp_var);

	fprintf(fd,"SAGG");
	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%11.2f",temp_var);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%9.2f",temp_var);

	fscanf(fs,"%d",&i);
	fprintf(fd,"%8d",i);

	fscanf(fs,"%d",&i);
	fprintf(fd,"%10d",i);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%14.2f",temp_var);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%12.2f",temp_var);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%10.2f\n",temp_var);

	fprintf(fd,"LAGG");
	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%11.2f",temp_var);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%9.2f",temp_var);

	fscanf(fs,"%d",&i);
	fprintf(fd,"%8d",i);

	fscanf(fs,"%d",&i);
	fprintf(fd,"%10d",i);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%14.2f",temp_var);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%12.2f",temp_var);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%10.2f\n",temp_var);

	fprintf(fd,"SAND");
	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%11.2f",temp_var);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%9.2f",temp_var);

	fscanf(fs,"%d",&i);
	fprintf(fd,"%8d",i);

	fscanf(fs,"%d",&i);
	fprintf(fd,"%10d",i);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%14.2f",temp_var);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%12.2f",temp_var);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%10.2f\n",temp_var);

	fprintf(fd,"______________________________________________________________________________\n");
	fprintf(fd,"TOTAL");
	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%10.2f",temp_var);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%9.2f",temp_var);

	fscanf(fs,"%d",&i);
	fprintf(fd,"%8d",i);

	fscanf(fs,"%d",&i);
	fprintf(fd,"%10d",i);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%14.2f",temp_var);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%12.2f",temp_var);

	fscanf(fs,"%f",&temp_var);
	fprintf(fd,"%10.2f\n",temp_var);

	fclose(fd);
	fclose(fs);

	system("enscript -h -B -Paa3 temp.nps");
	system("rm temp.nps");
}
