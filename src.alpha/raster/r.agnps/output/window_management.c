
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

#include "map_gen.h"
#include "window_management.h"

int window_management(choice)
int	choice;
{

	int t, b, l, r ;
	int i, option, inc_ew_top, inc_ew_bot, left;
	struct Cell_head window;
	char	temp[128], buf[128];
	int	cell_in_out_option(), cell_value, choice1;
	float	temp1, temp2;
	extern void working_sngl();
        int G_system(), G_fatal_error(), G_put_window(), G_set_window();
        int G__get_window(), G_yes(), G_gets();
        int G_clear_screen(), G_warning();
        int D_get_cur_wind(), D_set_cur_wind(), D_check_map_window();
        int D_get_screen_window(), D_new_window(), Dchoose(), Derase();
        int R_open_driver(), R_close_driver();
        int show_maps(), cell_input_option();
        int display_map(), wshd_summary(), save_maps(), user_view();
        int cr_usr_maps(), dis_usr_cr_maps(), display_where();
        int usr_display_opt(), usr_display(), cdf_var_opt(), cdf_incr_opt();
        int cdfx(), cdfy(), win_label(), line();
        int in_wind(), histogram(), what();
        int usr_avg_stats(), compute_avg_stats(), wshd_wind();

	if(choice == soil_loss) {
		num_windows = 7;
		num_top_row_win = 4;
		num_bot_row_win = 2;
		}
	else if(choice == nutrients) {
		num_windows  = 6;
		num_top_row_win = 3;
		num_bot_row_win = 2;
		}
	else if(choice == feedlot) num_windows  = 7;
	else if(choice == runoff){
		num_windows  = 7;
		num_top_row_win = 4;
		num_bot_row_win = 2;
		}

	G_system("d.frame -e");

	R_open_driver();
	
	/*
	b = R_screen_bot();
	t = R_screen_top();
	l = R_screen_left();
	r = R_screen_rite();
	*/


	if (D_get_cur_wind(temp))
		G_fatal_error("No current graphics window") ;

	if (D_set_cur_wind(temp))
		G_fatal_error("Current graphics window not available") ;

	/* Read in the map window associated with window */
/*
	G_get_window(&window) ;

	if (D_check_map_window(&window))
		G_fatal_error("Setting graphics window") ;
*/

	G_put_window(&orig_window) ;

	if (G_set_window(&orig_window) == -1)
		G_fatal_error("Can't set current graphics window") ;
	if (D_check_map_window(&orig_window))
		G_fatal_error("Setting graphics window") ;

	D_get_screen_window(&t,&b,&l,&r);


	inc_ew_top = (int) (r-l)/num_top_row_win;
	inc_ew_bot = (int) (r-l)/(num_bot_row_win+2);

	left = l;


	for(i = 0; i< num_windows; i++) sprintf(w_name[i].window_name,"wind%d",i);

	printf("%s\n",w_name[0].window_name);

	for(i = 0; i< num_top_row_win-1; i++){
		D_new_window(w_name[i].window_name,t+1,(int) (((b-t)/2)-1),l+1,l = l+inc_ew_top-1);
		}

		D_new_window(w_name[num_top_row_win-1].window_name,t+1,(int) (((b-t)/4)-1),l+1, l+inc_ew_top-1);

		D_new_window(w_name[num_top_row_win].window_name,(int) (((b-t)/4)+1),(int) (((b-t)/2)-1),l+1, l+inc_ew_top-1);

	for(i = num_top_row_win+1; i< num_windows; i++)
		D_new_window(w_name[i].window_name,(int)(((b-t)/2)+1),b-1,left+1,left = left+inc_ew_bot-1);
		D_new_window(w_name[num_windows].window_name,(int)(((b-t)/2)+1),b-1,left+1,left = left+2*inc_ew_bot-1);

	R_close_driver();

	wshd_view = NO;
	working_sngl();
	show_maps(orig_window,choice,cur);

	for(;;){
	option = cell_in_out_option();
	   if(option == 4 && wshd_view == NO) wshd_view = YES;
	   else if(option == 4 && wshd_view == YES) wshd_view = NO;
	
	switch(option) {
	case 1:
		printf("Please choose a window to zoom\n");
		G_system("d.frame -s");
		G_system("d.rast.zoom");
/* make sure the new window is read and set before updateing the screen and maps */
		G__get_window(&window,"","WIND",G_mapset());
		if (G_set_window(&window) == -1)
			G_fatal_error("Can't set current graphics window") ;
		working_sngl();
		show_maps(window,choice,cur);
		break;

	case 2:
		option = cell_input_option();
		switch(option){
		case 1:
			for(;;){
			G_clear_screen();
			printf("\n\nPlease enter the cell number between 1 - %d --> ",no_cells[cur]);
			scanf("%d",&cell_value);
			if(cell_value > 0 && cell_value <= no_cells[cur]) break;
			}

			working_sngl();

			R_open_driver();
			Dchoose(w_name[num_top_row_win+1].window_name);
			Derase("black");
			R_close_driver();
			
			in_wind(cell_value,cur);


			R_open_driver();
			Dchoose(w_name[num_top_row_win+2].window_name);
			Derase("black");
			R_close_driver();
			histogram(cell_value,choice,cur);
			break;
		case 2:
			printf("Please choose a window to view a cell output\n");
			G_system("d.frame -s");
			for(;;){
				cell_value = what(cell_num_map->p);
				if(cell_value > 0 && cell_value <= no_cells[cur]) break;
				else G_warning("Please choose a cell from the displayed map of chosen window");
			}

			working_sngl();

			R_open_driver();
			Dchoose(w_name[num_top_row_win+1].window_name);
			Derase("black");
			R_close_driver();
			
			in_wind(cell_value,cur);

			R_open_driver();
			Dchoose(w_name[num_top_row_win+2].window_name);
			Derase("black");
			R_close_driver();
			histogram(cell_value,choice,cur);
			break;
			}
		break;

	case 3:
		printf("Please choose a window to view a cell output\n");
		G_system("d.frame -s");
		working_sngl();

		G__get_window(&window,"","WIND",G_mapset());
		if (G_set_window(&window) == -1)
			G_fatal_error("Can't set current graphics window") ;

		usr_avg_stats(window);
		compute_avg_stats(cur);

		R_open_driver();
		Dchoose(w_name[num_top_row_win+2].window_name);
		Derase("black");
		R_close_driver();
					 
		histogram(no_cells[cur]+2,choice,cur);
						 
		R_open_driver();
		Dchoose(w_name[num_top_row_win+1].window_name);
		Derase("black");
		R_close_driver();
		
		in_wind(no_cells[cur]+2,cur);

		break;
	case 4:
		if(wshd_view == YES)
		   wshd_wind(w_name[num_top_row_win].window_name,cell_num_map->p);
		else if(wshd_view == NO){
		    G__get_window(&window,"","WIND",G_mapset());
		    display_map(w_name[num_top_row_win].window_name,window,wshd_aspect,"Flow direction map",5);	 
		    }
		break;

	case 5:
		wshd_summary(cur);
		break;

		
	case 6:
		save_maps(choice);
		user_view(choice,cur);
		cr_usr_maps(orig_max,orig_min,choice,cur);
		dis_usr_cr_maps(orig_max,orig_min,choice);
		usr_modified = YES;	
		break;
	case 7:
		option = display_where();
		if(option == 1){
		   printf("Please choose a window to display map\n");
		   G_system("d.frame -s");
		   option = usr_display_opt();
		   working_sngl();
		   usr_display(option);
		   }
		else{
		   printf("Create a new window \n");
		   G_system("d.frame -c");
		   R_open_driver();
		   Derase("black");
		   R_close_driver();
		   option = usr_display_opt();
		   working_sngl();
		   usr_display(option);
		   }
		   break;
	case 8:
		   printf("Please choose a window to display the stats\n");
		   G_system("d.frame -s");
		   G_system("d.erase");
		   option = cdf_var_opt();
		   if(!G_yes("Would you like to keep the previous X-axis intervals",0)){
		   choice1 = cdf_incr_opt();
		   cdfx(option,choice1,cur);
		   }
		   cdfy(option,cur);
		   printf("Please Enter a legend for the graph -->");
		   while(!G_gets(buf));
		   printf("\n\nPlease Enter a label for X-AXIS -->");
		   while(!G_gets(temp));
		   win_label(buf);
		   R_open_driver();
		   line(temp,YES); /*YES draw Y-axis tics and units */
		   R_close_driver();
		   temp1 = Y[0];
		   temp2 = Y[1];
		   for(i = 1; i< no_X-1; i++){
			Y[i] = temp2-temp1;
			temp1 = temp2;
			temp2 = Y[i+1];
			}
			Y[no_X-1] = Y[no_X-1] - temp1;
		   R_open_driver();
		   line(temp,NO);
		   R_close_driver();
		   break;
	case 9:
		working_sngl();
		window_management(choice);
		break;
	case 10:
		working_sngl();
		G_system("myg");
		break;
	case 11:
		if(usr_modified == YES) save_maps(choice);
		return 0;
		}
	}


   return 0;
}


int d_number()
{
	char buf[100];
        int G_system();

	sprintf(buf,"d.rast.num %s",cell_num_map->p);

	G_system(buf);
        return 0;
}

int d_arrow(type)
int	type;
{

        int G_system();
	char buf[200];
        /* arrow(type); */

	sprintf(buf,"d.rast.arrow map=%s type=agnps arrow_color=black grid_color=black x_color=white unknown_color=white",wshd_aspect);

	G_system(buf);
        return 0;
}

int show_maps(window,choice,j)
	struct Cell_head window;
	int	choice;
	int	j;
{
  int Dchoose(), Derase();
  int R_open_driver(), R_close_driver();
  int display_map(), draw_scale(), in_wind(), histogram();

	if(choice == soil_loss){

	display_map(w_name[0].window_name,window,sed_in,"Erosion in tons",1);	
	draw_scale(sed_in,gen_above_max[j],gen_above_min[j]);
	display_map(w_name[1].window_name,window,sed_gen,"Deposition in tons",2);	
	draw_scale(sed_gen,within_max[j],within_min[j]);
	display_map(w_name[2].window_name,window,sed_out,"Sediment leaving cell in tons",3);	
	draw_scale(sed_out,yield_max[j],yield_min[j]);
	}

	else if(choice == runoff){

	display_map(w_name[0].window_name,window,ro_us,"Runoff from Upstream in inches",1);	
	draw_scale(ro_us,ro_us_max[j],ro_us_min[j]);
	display_map(w_name[1].window_name,window,ro_gen,"Runoff generated in inches",2);	
	draw_scale(ro_gen,ro_gen_max[j],ro_gen_min[j]);
	display_map(w_name[2].window_name,window,ro_ds,"Runoff to Downstream in inches",3);	
	draw_scale(ro_ds,ro_ds_max[j],ro_ds_min[j]);
	}

	else if(choice == nutrients){
	if(NUTRIENT == N && NUT_ATTCH == sed){
	   display_map(w_name[0].window_name,window,N_sed_in,"Total N in Sediment generated in lbs per ac",1);	
	   draw_scale(N_sed_in,N_sed_in_max[j],N_sed_in_min[j]);
	   display_map(w_name[1].window_name,window,N_sed_out,"Total N in Sediment leaving in lbs per ac",2);	
	   draw_scale(N_sed_out,N_sed_out_max[j],N_sed_out_min[j]);
	}
	else if(NUTRIENT == N && NUT_ATTCH == ro){
	   display_map(w_name[0].window_name,window,N_ro_in,"Total N in Runoff generated in lbs per ac",1);	
	   draw_scale(N_ro_in,N_ro_in_max[j],N_ro_in_min[j]);
	   display_map(w_name[1].window_name,window,N_ro_out,"Total N in Runoff leaving in lbs per ac",2);	
	   draw_scale(N_ro_out,N_ro_out_max[j],N_ro_out_min[j]);
	}
	else if(NUTRIENT == P && NUT_ATTCH == sed){
	   display_map(w_name[0].window_name,window,P_sed_in,"Total P in Sediment generated in lbs per ac",1);	
	   draw_scale(P_sed_in,P_sed_in_max[j],P_sed_in_min[j]);
	   display_map(w_name[1].window_name,window,P_sed_out,"Total P in Sediment leaving in lbs per ac",2);	
	   draw_scale(P_sed_out,P_sed_out_max[j],P_sed_out_min[j]);
	}
	else if(NUTRIENT == P && NUT_ATTCH == ro){
	   display_map(w_name[0].window_name,window,P_ro_in,"Total P in Runoff generated in lbs per ac",1);	
	   draw_scale(P_ro_in,P_ro_in_max[j],P_ro_in_min[j]);
	   display_map(w_name[1].window_name,window,P_ro_out,"Total P in Runoff leaving in lbs per ac",2);	
	   draw_scale(P_ro_out,P_ro_out_max[j],P_ro_out_min[j]);
	}
	else if(NUTRIENT == COD ){
	   display_map(w_name[0].window_name,window,COD_ro_in,"Total COD in Runoff generated in lbs per ac",1);	
	   draw_scale(COD_ro_in,COD_ro_in_max[j],COD_ro_in_min[j]);
	   display_map(w_name[1].window_name,window,COD_ro_out,"Total COD in Runoff leaving in lbs per ac",2);	
	   draw_scale(COD_ro_out,COD_ro_out_max[j],COD_ro_out_min[j]);
	}
    }

	display_map(w_name[num_top_row_win-1].window_name,window,cell_num_map->p,"Cell number map",4);	
	if(wshd_view == NO)
	   display_map(w_name[num_top_row_win].window_name,window,wshd_aspect,"Flow direction map",5);	 

	R_open_driver();
	Dchoose(w_name[num_top_row_win+1].window_name);
	Derase("black");
	R_close_driver();

	in_wind(outlet_cell[cur],cur);

	R_open_driver();
	Dchoose(w_name[num_top_row_win+2].window_name);
	Derase("black");
	R_close_driver();

	histogram(outlet_cell[cur],choice,cur);
        return 0;
}
