#include "gis.h"
#define INCRBY 10
extern char streammap[300];
extern char streamanalysismap[200];
extern char basinanalysismap[200];
extern int no_of_basins;
int
go_select_basins(num)
int num;
{
	char *mapset;
	int button;
	int i,j;
	int signal;
	int ucount,qualified_clicks=0;
	int row,col;
	int nrows,ncols;
	int t,b,l,r;
	int screen_x,screen_y;
	int *sel_basins,count = 0;
	int mindex;
	struct Cell_head window;
	char frame_name[150];
	char filetemp[300],mapname[150];
	char command[600];
	double east,north;
	double D_get_d_north(), D_get_d_south();
	double D_get_d_east(), D_get_d_west();   
	double D_d_to_u_row(),D_d_to_u_col();
	CELL *buf;
	int fd;           
	FILE *fptmp;
	
	R_open_driver();

	mindex = INCRBY;
	sel_basins = (int *) calloc(INCRBY,sizeof(int));
	if(sel_basins == NULL){
		fprintf(stderr,"Insufficient Memory\n");
		exit(1);
	}

	if(D_get_cell_name(mapname)){
		fprintf(stderr,"warning: No data layer drawn in the current window\n");
		exit(-1);
	}

	if(D_get_cur_wind(frame_name))
		G_fatal_error("No Current graphics window");
	
	if(D_set_cur_wind(frame_name))
		G_fatal_error("Current Graphics window not avavilable");

	G_get_set_window(&window);

	if(D_check_map_window(&window))
		G_fatal_error("Setting graphics window");

	if(G_set_window(&window) == -1)
		G_fatal_error("Can't set current graphics window");

	if(D_get_screen_window(&t,&b,&l,&r))
		G_fatal_error("Getting graphics window coordinates");

	if(D_do_conversions(&window,t,b,l,r))
		G_fatal_error("Error in calculating conversions");


	mapset = G_find_cell(mapname, "");
	fd = G_open_cell_old(mapname, mapset);
	if(fd == NULL){
		fprintf(stderr,"Raster map cell file cannot be opened\n");
		exit(1);
	}

	nrows= window.rows;
	ncols= window.cols;
	buf = G_allocate_cell_buf();

	screen_x = ((int)D_get_d_west() + (int)D_get_d_east())/2;
	screen_y = ((int)D_get_d_north() + (int)D_get_d_south())/2;


	fprintf(stderr,"\nButtons\n Left: select basin\n Right: quit\n");

	do{
		R_get_location_with_pointer(&screen_x,&screen_y,&button);
		if(1)
		{
			if(button == 2) continue;
			if(button == 3) break;
		}
		east = D_d_to_u_col((double)screen_x);
		north = D_d_to_u_row((double)screen_y);
		row = (window.north - north)/window.ns_res;
		col = (east - window.west)/window.ew_res;

		if(row < 0 || row >= nrows) continue;
		if(col < 0 || col >= ncols) continue;

		if( G_get_map_row(fd,buf,row) >= 0 && buf[col] != 0){
			printf("Basin <%d> selected\n",buf[col]);
			qualified_clicks++;
		}

		if(count == mindex){
			mindex += INCRBY;    
			sel_basins = (int *) realloc(sel_basins, mindex * sizeof(int));
			if(sel_basins == NULL){
				fprintf(stderr,"Cannot resize memory\n");
				exit(2);
			}
		}
		if(buf[col] != (CELL) 0)	
			sel_basins[count++] = buf[col];
	}while(1);

	R_close_driver();
	/* To create Reclass file, sel_basins is made unique */
	unique(sel_basins,&ucount,count);   
	if(ucount == num){
		fprintf(stderr,"[No basins selected!]\n");
		exit(-1);
	}

	if(qualified_clicks == 0)
		return(0);

	strcpy(filetemp,tmpnam(NULL));
	fptmp = fopen(filetemp,"w");
	if(fptmp == NULL){
		fprintf(stderr,"Temporary File creation error\n");
		exit(2);
  	}
	if(num == 0){
		for(i=0;i< ucount; i++){
			fprintf(fptmp,"%d = %d\n",sel_basins[i],i+1); 
			fflush(fptmp);
		}
	}
	else{
		int reclass_val = 1;
		for(i=0;i<num;i++){
			signal = 0;
			for(j=0;j<count;j++){
				if((i+1) == sel_basins[j])
					signal = 1;
			}
			if(signal == 0){
				fprintf(fptmp,"%d = %d\n",i+1,reclass_val++);
				fflush(fptmp);
			}
		}
		ucount = reclass_val - 1;
		strcpy(mapname,"T_E_M_P_M_A_P");
		strcpy(streammap,"T_E_M_P_M_A_P2");
	}
	fclose(fptmp);

	sprintf(command,"r.reclass input=%s output=%s < %s",mapname,basinanalysismap,filetemp);
	system(command);
	sprintf(command,"d.frame -s r.water.fea1");
	system(command);
	sprintf(command,"d.erase");
	system(command);
	fprintf(stderr,"\nThese are the basins that you have selected.\n\n");
	sprintf(command,"d.rast %s",basinanalysismap);
	system(command);
	sprintf(command,"r.reclass input=%s output=%s < %s",streammap,streamanalysismap,filetemp);
	system(command);
	sprintf(command,"d.frame -s r.water.fea2");
	system(command);
	sprintf(command,"d.erase");
	system(command);
	fprintf(stderr,"\nThese are the corresponding streams.\n\n");
	sprintf(command,"d.rast %s",streamanalysismap);
	system(command);

	remove(filetemp);
	if(G_yes("Your stream network should be continuous.\nDo you wish to delete any basins ?\n", -1)){
		sprintf(command,"g.copy rast=%s,T_E_M_P_M_A_P > /dev/null",basinanalysismap);
		system(command);
		sprintf(command,"g.copy rast=%s,T_E_M_P_M_A_P2> /dev/null",streamanalysismap);
		system(command);
		fprintf(stderr,"Please select the basins that you wish to delete.\n");
		no_of_basins = ucount;
		return(1);
	}
	else{
		strcpy(streammap,streamanalysismap);
		no_of_basins = ucount;
		return(0);
	}
}
