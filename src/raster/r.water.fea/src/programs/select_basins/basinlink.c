#include "gis.h"
extern char element[64];
extern char streamanalysismap[200];
extern char basinanalysismap[200];

int absolute(value)
int value;
{
	if(value < 0) return (-1*value);
	else return value;
}

void 
basinlink(no_of_basins,accum_map,aspectmap)
int no_of_basins;
char *accum_map,*aspectmap;
{
	char *mapset;
	struct Cell_head window;
	int basin_no;
	struct basinlink{
		CELL aspect;
		int  frow;
		int  fcol;
		int  count;
	} *cr; 
	int *tobasin,*fbasin,*sn,*prev_accum;
		
	FILE *fib,*fptmp,*fptmp1,*fpbi;
	char filetemp[300],filetemp1[300];
	int fds,fdac,fda,fdb;
	int countsn = 0,countcells = 0,bn;
	CELL *streambuf,*accumbuf,*aspectbuf,*basinbuf;
	int row,col;
	int i,j,k;
	int signal = 0;
	double G_col_to_easting(), G_row_to_northing(); 
	fbasin  = (int *) calloc(no_of_basins,sizeof(int));
	tobasin = (int *) calloc(no_of_basins,sizeof(int));
	sn      = (int *) calloc(no_of_basins,sizeof(int));
	prev_accum = (int *) calloc(no_of_basins,sizeof(int));
	if(tobasin == NULL||fbasin == NULL||sn == NULL||prev_accum == NULL){
		fprintf(stderr,"Insufficient memory\n");
		exit(1);
	}
	for(i=0;i<no_of_basins;i++)
		prev_accum[i] = 0;

	mapset = G_find_cell(streamanalysismap,"");
	fds  = G_open_cell_old(streamanalysismap,mapset);
	mapset = G_find_cell(accum_map,"");
	fdac = G_open_cell_old(accum_map,mapset);
	mapset = G_find_cell(aspectmap,"");
	fda  = G_open_cell_old(aspectmap,mapset);
	mapset = G_find_cell(basinanalysismap,"");
	fdb  = G_open_cell_old(basinanalysismap,mapset);
	if(fds == NULL || fdac == NULL|| fda == NULL|| fdb == NULL){
		fprintf(stderr,"Raster maps can not be opened\n");
		exit(2);
	}

	fib = G_fopen_new(element,"input.basin");
	if(fib == NULL){
		fprintf(stderr,"File input.basin cannot be opened\n");
		exit(2);
	}
	fpbi = G_fopen_new(element,"basin_info");
	if(fpbi == NULL){
		fprintf(stderr,"File basin_info cannot be opened\n");
		exit(2);
	}

	cr = (struct basinlink *) calloc((no_of_basins + 1),sizeof(struct basinlink));
	if(cr == NULL){
		fprintf(stderr,"Insufficient memory\n");
		exit(1);
	}

	G_get_set_window(&window);

	streambuf = G_allocate_cell_buf();
	accumbuf  = G_allocate_cell_buf();
	aspectbuf = G_allocate_cell_buf();
	basinbuf  = G_allocate_cell_buf();
	if(streambuf == NULL||accumbuf == NULL||aspectbuf == NULL||basinbuf == NULL){
		fprintf(stderr,"Insufficient memory\n");
		exit(1);
	}
	fprintf(stderr,"Building Channel Topology ......\t\t"); 
	for(row=0;row < window.rows;row++){
		G_percent(row,window.rows,1);
		G_get_map_row(fds,streambuf,row);
		G_get_map_row(fdac,accumbuf,row);
		G_get_map_row(fda,aspectbuf,row);
		G_get_map_row(fdb,basinbuf,row);

		for(col=0;col < window.cols;col++){
			if(basinbuf[col] != 0){
				bn = basinbuf[col];
				(cr[bn].count)++;
				countcells++;
			}
			if(streambuf[col] != 0){
				basin_no = (int)streambuf[col];
				if(absolute((int)accumbuf[col]) > prev_accum[basin_no]){
					prev_accum[basin_no] =(int)absolute((int) accumbuf[col]);
					cr[basin_no].frow  =  row;
					cr[basin_no].fcol  =  col;
					cr[basin_no].aspect =  aspectbuf[col];
				}
			}
		}
	}

	/* The Maximum accumulation values are now collected */
	strcpy(filetemp,tmpnam(NULL));
	fptmp = fopen(filetemp,"w");
	if(fptmp == NULL){
		fprintf(stderr,"\nTemporary File creation error\n");
		exit(2);
	}
	tmpnam(filetemp1);
	fptmp1 = fopen(filetemp1,"w");
	if(fptmp1 == NULL){
		fprintf(stderr,"\nTemporary File creation error\n");
		exit(2);
	}

	fprintf(fib,"%d\n",no_of_basins  + 1 );
	fprintf(fpbi,"%d\n",no_of_basins);

	for(i=1; i<= no_of_basins; i++){
		switch(cr[i].aspect){
			case 1:
					row = cr[i].frow - 1;
					col = cr[i].fcol + 1;
					break;
			case 2:
					row = cr[i].frow - 1;
					col = cr[i].fcol;
					break;
			case 3:
					row = cr[i].frow - 1;
					col = cr[i].fcol - 1;
					break;
			case 4:
					row = cr[i].frow ;
					col = cr[i].fcol - 1;
					break;
			case 5:
					row = cr[i].frow + 1;
					col = cr[i].fcol - 1;
					break;
			case 6:
					row = cr[i].frow + 1;
					col = cr[i].fcol;
					break;
			case 7:
					row = cr[i].frow + 1;
					col = cr[i].fcol + 1;
					break;
			case 8:
					row = cr[i].frow ;
					col = cr[i].fcol + 1;
					break;
		}
		G_get_map_row(fds,streambuf,row);
		fprintf(fptmp,"%8.2lf %8.2lf\n",G_col_to_easting((double) col + 0.5,&window),G_row_to_northing((double) row + 0.5,&window));
		fprintf(fptmp1,"move %8.2lf %8.2lf\ntext %d\n",G_col_to_easting((double) col + 0.5,&window),G_row_to_northing((double) row + 0.5,&window),streambuf[col]);
		fflush(fptmp);
		fflush(fptmp1);
		fprintf(fib,"%d       %d\n",i,streambuf[col]);
		fprintf(fpbi,"%d %lf %d\n",i,cr[i].count * window.ns_res * window.ew_res,cr[i].count);

		fbasin[i - 1]  = i;
		tobasin[i - 1] = streambuf[col];
	}	 
	/* Finding starting nodes, Nodes that are only in from basin */
		for(k=0;k<no_of_basins;k++){ 
			 signal = 0;
			 for(j=0;j<no_of_basins;j++){
				 if(fbasin[k] == tobasin[j])
					 signal = 1;
			 }
			 if(signal == 0)
				 sn[countsn++] = fbasin[k];
		 }
		 fprintf(fib,"%d\n",countsn );
		 for(i=0; i< countsn; i++)
			 fprintf(fib,"%d\n",sn[i]);
	  	 fprintf(fib,"%f\n",countcells * window.ew_res * window.ns_res);	
		 {
			char command[500];
			sprintf(command,"d.frame -s r.water.fea2");
			system(command);
			sprintf(command,"d.points color=black size=8 file=%s",filetemp); 
			system(command);
			sprintf(command,"d.mapgraph input=%s color=magenta",filetemp1); 
			system(command);
			sprintf(command,"d.frame -s r.water.fea1");
			system(command);
	     }
		 fclose(fptmp);
		 fclose(fptmp1);
		 remove(filetemp);
		 remove(filetemp1);
		 fclose(fib);
		 fclose(fpbi);
		 fprintf(stderr,"100%% Completed\n");
}
