/* This program reads the data and writes it to a file, which is later    
used by the timedata.c subroutine of finite element program */
#include "gis.h"
#include <math.h>
#define TRUE 1
#define FALSE 0

static char timefile[64]="NOFILE";
static char mapname[64] = "NOMAP";
static char element[64] = "r.water.fea/";
static int ret;
static FILE *fpp;
/*************************************************************************
* Calculates length from the  data about linked topology generated from  *
*  drainage map			                                         * 
*************************************************************************/
void
lengthfind(aspectmap,streammap)
char *aspectmap, *streammap;
{
	char line[30];
	char *mapset;
	int i,j,max;
	double nslope,base,mannings;
	double hyp;
	int sn,*countx,*county,*counthyp;
	double length;
	int fd1, fd2;
	CELL *aspect_cell, *stream_cell;
	struct Cell_head window;

	FILE *fp1;

	fp1 = (FILE *) G_fopen_old(element,"input.basin",G_mapset());
	
	if(fp1 == NULL){
		fprintf(stderr,"Input.basin cannot be opened.\n");
		exit(1);
	}

	fscanf(fp1,"%d",&max);
	countx =(int *) calloc(max,sizeof(int));
	county =(int *) calloc(max,sizeof(int));
	counthyp =(int *) calloc(max,sizeof(int));
	if(countx == NULL|| county == NULL || counthyp == NULL){
		fprintf(stderr,"Memory allocation error!\n");
		exit (1);
	}
	G_get_set_window(&window);

	mapset = G_find_cell(aspectmap,"");
	fd1 = G_open_cell_old(aspectmap,mapset);
	mapset = G_find_cell(streammap,"");
	fd2 = G_open_cell_old(streammap,mapset);

	aspect_cell = G_allocate_cell_buf();
	stream_cell = G_allocate_cell_buf();

	hyp = sqrt((double)(window.ns_res * window.ns_res + window.ew_res * window.ew_res));
	/*calculating the lengths i= row index, j= column index*/
	for(i=0;i< window.rows;i++){

		G_get_map_row(fd1,aspect_cell, i);
		G_get_map_row(fd2,stream_cell, i);

      	for(j=0;j< window.cols;j++)
			if(stream_cell[j] != 0){
				sn = stream_cell[j];
				switch(aspect_cell[j]){
					case 1 :
						(counthyp[sn])++;
						break;
					case 2 :
						(county[sn])++;
						break;
					case 3 :
						(counthyp[sn])++;
						break;
					case 4 :
						(countx[sn])++;
						break;
					case 5 :
						(counthyp[sn])++;
						break;
					case 6 :
						(county[sn])++;
						break;
					case 7 :
						(counthyp[sn])++;
						break;
					case 8 :
						(countx[sn])++;
						break;
					default:
						fprintf(stderr,"ERROR - Wrong aspect value detected\n");
						break;
				}
			}
	}
	printf("Please enter channel charecteristics based on\nthe trapezoid shown on the monitor, enter one value for each stream\n");
	for(i=1;i<max;i++){
		length = countx[i] * window.ew_res + county[i] * window.ns_res + counthyp[i] * hyp;
	    printf("[Basin number: %d]\n",i);
	    do{
		printf("n= ");
		if(!gets(line)) exit(1);
		ret = sscanf(line,"%lf",&nslope);
		if(nslope < 0.2){
			ret = FALSE;
			fprintf(stderr,"**<%g>**Invalid value\n",nslope);
		}
	    }while(ret != TRUE);
            do{	
		printf("W(meters)= ");
		if(!gets(line)) exit(1);
		 ret = sscanf(line,"%lf",&base);
		 if(base < 1.0){ 
			ret = FALSE;
			fprintf(stderr,"**<%g>**Invalid Value\n",base);
		}
	    }while(ret != TRUE);
	    do{
		printf("Mannings roughness[0.01-0.8]= ");
		if(!gets(line)) exit(1);
		ret = sscanf(line,"%lf",&mannings);
		if(mannings < 0.01 || mannings > 0.8){
			ret = FALSE;
			fprintf(stderr,"**<%g>**Invalid value\n",mannings);
		}
	    }while(ret != TRUE);
		fprintf(fpp,"C %d\t%lf\t%lf\t%lf\t%lf\n",i,length,nslope,base,mannings);
	}
	fclose(fp1);
}
main(argc,argv)
int argc;
char *argv[];
{
	int delta_t,duration,monit_time;
	int infilmaps=0;
	int cellcounts;
	char line[30];
	char timeelement[64];
	char *mapset;
	void lengthfind();
	double courant_time,rain_max;
	double max_head,tot_length,max_man,maxres,min_le;
	struct Cell_head window;

	struct{
		struct Flag *two; /* Timefile */
		struct Flag *one; /* Constant rate */
	} flag;

	struct{
		struct Option *project;
		struct Option *stream;
		struct Option *aspect;
	}option;


	 option.project = G_define_option();
	 option.project->key = "project";
	 option.project->type = TYPE_STRING;
	 option.project->required = YES;
         option.project->gisprompt = "any,r.water.fea,project";
	 option.project->description = "project name";

	 option.stream = G_define_option();
	 option.stream->key = "stream";
	 option.stream->type = TYPE_STRING;
	 option.stream->required = YES;
	 option.stream->gisprompt = "old,cell,raster";
	 option.stream->description = "stream segments map";

	option.aspect = G_define_option();
	option.aspect->key = "drainage";
	option.aspect->type = TYPE_STRING;
	option.aspect->required = YES;
	option.aspect->gisprompt = "old,cell,raster";
	option.aspect->description = "drainage direction map";

	flag.two = G_define_flag();
	flag.two->key = '2';
	flag.two->description = "Temporally varying rainfall";

	flag.one = G_define_flag();
	flag.one->key = '1';
	flag.one->description = "constant rainfall rate";

	G_gisinit(argv[0]);

	if(G_parser(argc,argv))
			return(-1);

	strcat(element,option.project->answer);

	G_get_set_window(&window);
	if(window.ns_res > window.ew_res){
		maxres = (double) window.ns_res;
		min_le = (double) window.ew_res;
	}
	else{
		maxres = (double) window.ew_res;
		min_le = (double) window.ns_res;
	}

	{
		char ba_map[75] ;
		strcpy(ba_map,"fea.basin.");
		strcat(ba_map,option.project->answer);
 		count_cells_max(ba_map,&cellcounts);
		tot_length = maxres * cellcounts;
	}

	fpp = G_fopen_new(element,"timedata");
	if(fpp == NULL){
		fprintf(stderr,"FILE OPENING ERROR!\n");
		return(15);
	}
	/* query the user for data */	
	printf("\nEnter the rainfall duration in minutes");
	do{
		printf("\nDURATION : ");
		if(!gets(line)) return (1);
		ret = sscanf(line,"%d", &duration);
		if(duration <= 0){
			ret = FALSE;
			fprintf(stderr,"**<%g>**Invalid value\n",duration);
		}
	}while(ret != TRUE);
/* calculate minimum time increment and store the value as courant_time */
	printf("\nEnter the maximum rainfall intensity(cm/hr) expected");
	do{
 		printf("\nMAX INTENSITY: ");
		if(!gets(line)) return (1);
		ret = sscanf(line,"%lf", &rain_max);
		if(rain_max <= 0.01){
			ret = FALSE;
			fprintf(stderr,"**<%g>**Invalid value\n",rain_max);
		}
	}while(ret != TRUE);

	/* Reading information from file */
	printf("\nPlease enter the maximum Manning's roughness value in your watershed");
	do{
		printf("\nMAX. MANNINGS[0.01-0.9]: ");
		if(!gets(line)) return(1);
		ret = sscanf(line,"%lf",&max_man); 
		if(max_man < 0.01 || max_man > 0.9){
			ret = FALSE;
			fprintf(stderr,"**<%g>**Invalid value\n",max_man);
		 }
	}while(ret != TRUE);
   
	/* 360000 is the conversion factor -->> cm/hr to m/sec, 0.6 comes because
	0.6 is the same as 3/5*/
	max_head = pow((double)((tot_length * rain_max * max_man)/(360000 * sqrt(0.02))),(double) 0.6); /* 2 percent minimum slope */
	courant_time = min_le/sqrt((double)(9.81 * max_head));
	if(courant_time < 4.0){
		courant_time = 4.0;
	}
	printf("\nCOURANT TIME: %lf seconds\n",courant_time);
	printf("\nFor numerical stability use %d seconds to %d seconds as time increment\n",(int)(0.5*courant_time),(int)(0.75*courant_time));
	printf("\nEnter time increment at which calculation should be made");
	do{
 		printf("\nTIME INCREMENT (seconds): ");
		if(!gets(line)) return(1);
		ret = sscanf(line,"%d",&delta_t);
		if(delta_t <= 0 || delta_t > courant_time){
			ret = FALSE;
		 	fprintf(stderr,"**<%g>**Invalid value\n",delta_t);
		 }
	}while(ret != TRUE);
	printf("\nEnter the length of time for which you would like to monitor the hydrograph");
	do{
		printf("\nMONITOR TIME (minutes): ");
		if(!gets(line)) return(1);
		ret = sscanf(line,"%d",&monit_time);
		if(monit_time <= 0||monit_time*60 < delta_t){
			ret = FALSE;
			fprintf(stderr,"**<%d>**Invalid value\n",monit_time);
		}
	 }while(ret != TRUE);
	/* query for the file and then check if it exists */
	if(flag.two->answer == 1){
		sprintf(timeelement,"%s/timefiles",element);
		G__make_mapset_element(timeelement);
		mapset = G_ask_any("Enter the name of the time file",timefile,timeelement,"",0);
		if(mapset == NULL)
			exit(0);
		else{
			if(G_find_file(timeelement,timefile,mapset) == NULL){
				fprintf(stderr,"[ File <%s> does not exist, a new one will be created.]\n",timefile);
				timefilecreate(timefile,timeelement);
			}
		}
	}

	fprintf(fpp,"DURATION: %d\n",duration);
	fprintf(fpp,"MAX_INTENSITY: %lf\n",rain_max);
	fprintf(fpp,"TIME_INCREMENT: %d\n",delta_t);
	fprintf(fpp,"MONITORING_TIME: %d\n",monit_time);
	fprintf(fpp,"TIMEFILE: %s\n",timefile);

	G_system("clear");	
	if(G_yes("\nDo you have maps of the following types?\n\n 1.Mannings roughness coefficient.\n 2.Saturated conductivity values(m/sec).\n 3.Capillary suction values(m).\n 4.Porosity values(%)\n\nPlease answer yes only if you have all the maps\n",-1)){
		infilmaps = 1; /* yes the maps are there */
		fprintf(fpp,"INFILMAPS: %d\n",infilmaps);
	}
	else
		fprintf(fpp,"INFILMAPS: %d\n",infilmaps);

	if(infilmaps == 1){
		confirm_map("MANNINGS MAP", mapname);
		fprintf(fpp,"MANNINGS_MAP: %s\n",mapname);
		confirm_map("SATURATED CONDUCTIVITY MAP", mapname);
		fprintf(fpp,"SAT_CONDUCTIVITY_MAP: %s\n",mapname);
		confirm_map("CAPILLARY SUCTION MAP", mapname);
		fprintf(fpp,"CAPILLARY_SUCTION_MAP: %s\n",mapname);
		confirm_map("POROSITY MAP", mapname);
		fprintf(fpp,"POROSITY_MAP: %s\n",mapname);
	}
	fflush(fpp);

	lengthfind(option.aspect->answer,option.stream->answer);
	fclose(fpp);
	return(0);
}
