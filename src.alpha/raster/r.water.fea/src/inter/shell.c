#include "rfea.h"
#include "gis.h"
#define MAXANIMATE 40
#define MAX_LINES 41
      
void
change_column(no_of_basins,change_basin)
int no_of_basins,change_basin;
{
	int i,lines=0;
	char *tempname;
	float new_discharge,old_discharge;
	FILE *fpr1,*fpr2,*fptmp;

	fpr1 = (FILE *)G_fopen_old(element,"disch.basin",G_mapset());
	fpr2 = (FILE *)G_fopen_old(element,"discharge.basin",G_mapset());
	if(fpr1 == NULL || fpr2 == NULL ){
		fprintf(stderr,"FIle opening error\n");
		exit(1);
	}

	tempname = (char *) G_tempfile();
	fptmp = fopen(tempname,"w");
	if(fptmp==NULL){
		fprintf(stderr,"temporary file creation error");
		exit(1);
	}
	
	do{
		fscanf(fpr2,"%f",&new_discharge);
		lines++;
		for(i=1;i<no_of_basins;i++){
			fscanf(fpr1,"%f",&old_discharge);
			if(i==change_basin)
				fprintf(fptmp,"\t%f ",new_discharge);
			else
				fprintf(fptmp,"\t%f ",old_discharge);
		}
		fprintf(fptmp,"\n");
	}while(lines <= MAX_LINES);

	fclose(fpr1);
	fclose(fpr2);
	fclose(fptmp);
	G_remove(element,"disch.basin");
		
	{
		char comm[300];
		sprintf(comm,"mv %s %s/%s/%s/disch.basin",tempname,G_location_path(),G_mapset(),element);
		system(comm);
	}
}

void
fea_handler()
{ 
	FILE *fpib;
	int no_of_basins;
	int i;
	char unixcommand[500];
	char tempfile[200];
 	
	fpib = (FILE *)G_fopen_old(element,"input.basin",G_mapset());
	if(fpib == NULL){
		fprintf(stderr,"File opening Error\n");
		exit(1);
	}
	sprintf(unixcommand,"echo ' ' > %s/%s/%s/disch.basin",G_location_path(),G_mapset(),element);
	system(unixcommand);
	G_remove(element,"disch_file");
	fscanf(fpib,"%d",&no_of_basins);
	tmpnam(tempfile);
	sprintf(unixcommand,"d.frame -s r.water.fea1");
	system(unixcommand);
	fclose(fpib);

	for(i=1; i< no_of_basins; i++){
		fprintf(stderr,"[ANALYZING BASIN %d ............]\n",i);
			sprintf(unixcommand,"%s/etc/r.fea/feapro.bd %s %s pr=%s slope=%s basin_no=%d  mode=%s",G_gisbase(),fflag,mflag,project,slope,i,mmode);
			if(system(unixcommand)!= 0){
				exit(3);
			}
			sprintf(unixcommand,"paste %s/%s/%s/disch.basin %s/%s/%s/discharge.basin > %s",G_location_path(),G_mapset(),element,G_location_path(),G_mapset(),element,tempfile); 
			system(unixcommand);
			sprintf(unixcommand,"mv %s %s/%s/%s/disch.basin",tempfile,G_location_path(),G_mapset(),element);
			system(unixcommand);
	}
}

void
place_new_values(analysed_basin,tempfile)
char *tempfile;
int analysed_basin;
{
	char command[1024];
	char path[1024];
	int count=0,akchar;
	FILE *fptemp,*fp;

	fprintf(stderr,"[Updating basin animation map]\n");
	fptemp = (FILE *)fopen(tempfile,"a");
	fp = (FILE *)G_fopen_old(element,"disch_file",G_mapset());
	if(fptemp == NULL){
		fprintf(stderr,"File opening error - modify\n");
		exit(2);
	}

	if(fp == NULL){
		fprintf(stderr,"File opening error - disch_file\n");
		exit(2);
	}
	fseek(fptemp,0L,0);

	do{
		if((akchar =fgetc(fptemp)) == 'Z')
			count++;
		if(count == analysed_basin){ 
			fseek(fptemp,-1,0);
			break;
		}
	}while(akchar != EOF);
	/* Direct transfer */
	while((akchar = fgetc(fp)) != EOF){
		fputc(akchar, fptemp);
	}
	fclose(fptemp);
	fclose(fp);

	G__file_name (path, element, "disch_file", G_mapset());
	sprintf(command,"rm %s",path);
	system(command);

	sprintf(command,"mv %s %s", tempfile,path);
	system(command);

}

void
vary_basin_fea()
{
	char command[400],line[81],*tempfile;
	int no_of_basins,analyse_basin;
	int ret = 0;
	void place_new_values(),change_column();
	FILE *fpib;

	fpib = (FILE *)G_fopen_old(element,"input.basin",G_mapset());
	if(fpib == NULL){
		fprintf(stderr,"File opening Error\n");
		exit(1);
	}
	fscanf(fpib,"%d",&no_of_basins);
	fclose(fpib);
	
	tempfile = G_tempfile();
	sprintf(command,"mv %s/%s/%s/disch_file %s",G_location_path(),G_mapset(),element,tempfile);
	system(command);

	printf("Enter the basin number that you want to analyse\n");
	do{
		printf("Basin number:");
		if(!gets(line)) exit(3);
		ret=sscanf(line,"%d",&analyse_basin);
		if(analyse_basin > no_of_basins || analyse_basin < 0){
		    ret = -1;
		    fprintf(stderr,"**<%d>**Invalid value\n",analyse_basin);
		}
	}while(ret != 1);
	sprintf(command,"d.frame -s r.water.fea1");
	system(command);
	printf("[ANALYZING BASIN %d ............]\n",analyse_basin);
	sprintf(command,"%s/etc/r.fea/feapro.bd %s %s pr=%s slope=%s basin_no=%d mode=%s",G_gisbase(),fflag,mflag,project,slope,analyse_basin,mmode);
	if(system(command))
		exit(3);
	change_column(no_of_basins,analyse_basin);
	/* Change the actual values in the disch_file based on new values */
	if(strcmp(mflag,"-m") == 0)
		place_new_values(analyse_basin,tempfile);
}


/**************************************************************************
*  Program for making maps                                                *
*  disch_file =>> file name that contains the discharge values at each    * 
*  point in a basin.                                                      *
***************************************************************************/
#define M_FACTOR 10000

int
makemap(time_step)
int time_step;
{
	char command[100];
	char mapname[100];
	long current_pos;
	int i,max,akchar;
	int row,col;
	int in_window = 1;
	int out_fd;
	float discharge;
	double easting,northing;
	struct Cell_head window;
	struct Colors colors;
	struct Range range;
	double G_northing_to_row(), G_easting_to_col();
	CELL value,small,big;
	FILE *fpcoordinate,*fpdischarge;

	sprintf(mapname,"fea.%s.%.2d",project,time_step);
	fpcoordinate = (FILE *)G_fopen_old(element,"coordinates",G_mapset());
	fpdischarge = (FILE *)G_fopen_old(element,"disch_file",G_mapset());
	if(fpcoordinate == NULL|| fpdischarge == NULL ){
		fprintf(stderr,"File opening error!\n");
		exit(1);
	}

	out_fd = G_open_cell_new_random(mapname);
	if(out_fd == NULL)
	{
		fprintf(stderr,"%s - Cannot create raster file [%s]",G_program_name(),mapname);
		exit(1);
 	}

	G_get_set_window(&window);
	do{
		akchar = getc(fpdischarge);
		if(akchar == 'Z'){
			fscanf(fpdischarge,"%d",&max);
			current_pos = ftell(fpdischarge);
			fseek(fpdischarge,(long)(current_pos + (time_step * max * 9) + time_step + 1),0); 
			for(i=0;i<max;i++){
				fscanf(fpdischarge,"%f",&discharge);
				fscanf(fpcoordinate,"%lf %lf",&easting,&northing);
				value = (CELL) (discharge * M_FACTOR);
				row = (int) G_northing_to_row(northing,&window);
				col = (int) G_easting_to_col(easting,&window);
				if(row < 0||row >= window.rows||col < 0||col >= window.cols)
					in_window = 0;
					if(!in_window)
				fprintf(stderr,"**Note**%lf %lf is outside your current window\n",easting,northing);	
			else
				G_put_map_row_random(out_fd,&value,row,col,1);
			}
			getc(fpdischarge);
		}
	}while(akchar != EOF);
	fclose(fpcoordinate);
	fclose(fpdischarge);
	G_close_cell(out_fd);
	G_read_range(mapname,G_mapset(),&range);
	G_get_range_min_max(&range,&small,&big);
	G_init_colors(&colors);
	/* Write Color table */
		G_add_color_rule((CELL)0, 0,0,0,(CELL)(small),191,191,191 ,&colors);
		G_add_color_rule((CELL)(small), 191,191,191,(CELL)(0.1*big),0,191,191 ,&colors);
		G_add_color_rule((CELL)(.1*big), 0,191,191,(CELL)(0.4*big),0,255,255 ,&colors);
		G_add_color_rule((CELL)(.4*big), 0,255,255,(CELL)(0.7*big), 0,255,0,&colors);
		G_add_color_rule((CELL)(.5*big), 0,255,0,(CELL)big, 0,0,255,&colors);

	G_write_colors(mapname,G_mapset(),&colors);
	sprintf(command,"r.compress %s > /dev/null",mapname);
	system( command);
	return(0);
}			 

void
channelfea()
{
	char command[500];
	int i,duration;
	FILE *fptd;
	fptd = (FILE *)G_fopen_old(element,"timedata",G_mapset());
	if(fptd == NULL){
		fprintf(stderr,"timedata cannot be opened. \n");
		exit(8);
	}
	fscanf(fptd,"%*s%*s%*s%*s%*s%*s%*s%d",&duration);
	fclose(fptd);
	sprintf(command,"d.frame -s r.water.fea1");
	system(command);
	sprintf(command,"%s/etc/r.fea/cfeapro.bd pr=%s slope=%s",G_gisbase(),project,slope);
	if(system(command) != 0)
		exit(3);

	if(strcmp(mflag,"-m") == 0){
		sprintf(command,"d.frame -s r.water.fea2");
		system(command);
		sprintf(command,"d.erase");
		system(command);
		fprintf(stderr,"==>>[Starting animation]\n");
		for(i=2;i<=MAXANIMATE;i += 2){
			fprintf(stderr,"[Time step = %d]\nFlow depth after %d minutes since begining of rainfall.\n",i,(int)(duration*((1.0*i)/MAXANIMATE)));
			if(makemap(i))
				exit(1);
			sprintf(command,"d.rast fea.%s.%.2d",project,i);
			system(command);
		}
	}
	remove("discharge.basin");
}
