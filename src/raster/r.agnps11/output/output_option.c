
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

int	output_option()
{
	int	option = 1;
        int V_clear(), V_line(), V_ques(), V_call(), G_warning();

	for(;;){
	V_clear();
	V_line(1, "         Visualization Tool Main Menu");
	V_line(3, "         Output Display Options");
	V_line(5,"	    1. Watershed Summary including sediment (no graphics)");
	V_line(6,"	    2. Soil Loss (graphics)");
	V_line(7,"	    3. Nutrients Movement (graphics)");
	V_line(8,"	    4. Feedlot Analysis (graphics)");
	V_line(9,"	    5. Runoff Movement (graphics)");
	V_line(10,"	    6. Analyze Different Scenarios");
	V_line(11,"	    7. Save Output Maps");
	V_line(12,"	    8. Exit to GRASS (to come back type 'return') ");
	V_line(13,"	    9. Quit");
	V_line(15,"	    Enter the choice (1-9):");
	V_ques(&option,'i',15,40,1);

	if(!V_call()) exit(1);

	if(option > 0 && option <=9) break;
	else G_warning("Please enter one of the choices between 1-9");
	}
	return(option);

}

int	cell_in_out_option()
{
	int	option=9;
        int V_clear(), V_line(), V_ques(), V_call(), G_warning();
	
	for(;;){
	V_clear();
	V_line(1,"   Cell input and output Display Options");
	V_line(3,"   1. Zoom"); 
	V_line(4,"   2. View a Cell");
	V_line(5,"   3. View an area output");
	V_line(6,"   4. Toggle between flow direction map/viewing area");
	V_line(7,"   5. Show the watershed summary output");
	V_line(8,"   6. Displayed range of output maps ");
	V_line(9,"   7. Display user's choice map");
	V_line(10,"   8. Draw Cumulative and Frequency Distribution Stats");
	V_line(11,"   9. Restore the initial screen");
	V_line(12,"   10. Exit to GRASS (to come back type 'return') ");
	V_line(13,"   11. Return to Main Menu");
	V_line(14,"   Enter the choice (1-11):");
	V_ques(&option,'i',14,40,2);

	if(!V_call()) exit(1);
	
	if(option > 0 && option <=11) break;
	else G_warning("Please enter one of the choices between 1-11");
	}
	return(option);

}

int	cell_input_option()
{

	int	option=0;
        int V_clear(), V_line(), V_ques(), V_call(), G_warning();

	for(;;){
	V_clear();
	V_line(3, "         Would you like to enter the CELL number to display");
	V_line(7,"	    1. Yes"); 
	V_line(9,"	    2. No (mouse option use graphics monitor)");
	V_line(11,"	    Enter the choice (1-2):");
	V_ques(&option,'i',11,40,1);

	if(!V_call()) exit(1);
	
	if(option > 0 && option <=2) break;
	else G_warning("Please enter one of the choices between 1-2");
	}
	return(option);

}

int	nutrient_opt()
{
	int	option=0;
        int V_clear(), V_line(), V_ques(), V_call(), G_warning();

	for(;;){
	V_clear();
	V_line(3, "         Choose one of the Nutrient for diplaying the output ");
	V_line(7,"	    1. Nitrogen"); 
	V_line(9,"	    2. Phosphorous");
	V_line(11,"	    3. COD (Chemical Oxygen Demand)");
	V_line(13,"	    Enter the choice (1-3):");
	V_ques(&option,'i',13,40,1);

	if(!V_call()) exit(1);

	if(option > 0 && option <=3) break;
	else G_warning("Please enter one of the choices between 1-3");
	}
	
	if(option == 1) return(N);
	else if(option == 2) return(P);
	else if(option == 3) return(COD);

        return 0;
}
	
int	nutrient_attch_opt()
{
	int	option=0;
        int V_clear(), V_line(), V_ques(), V_call(), G_warning();

	for(;;){
	V_clear();
	V_line(3, "         Would you like to view the selected Nutrient attached to ");
	V_line(7,"	    1. Sediment"); 
	V_line(9,"	    2. Runoff");
	V_line(11,"	    Enter the choice (1-2):");
	V_ques(&option,'i',11,40,1);

	if(!V_call()) exit(1);

	if(option > 0 && option <=2) break;
	else G_warning("Please enter one of the choices between 1-2");
	}
	
	if(option == 1) return(sed);
	else if(option == 2) return(ro);

        return 0;
}

int	usr_display_opt()
{
	int	option=0;
        int V_clear(), V_line(), V_ques(), V_call(), G_warning();

	for(;;){
	V_clear();
	V_line(3, "         Would you like to display ");
	V_line(7,"	    1. Cell map"); 
	V_line(9,"	    2. Overlay map");
	V_line(11,"	    3. Vector map");
	V_line(13,"	    Enter the choice (1-3):");
	V_ques(&option,'i',13,40,1);

	if(!V_call()) exit(1);

	if(option > 0 && option <=3) break;
	else G_warning("Please enter one of the choices between 1-3");
	}
	
	return(option);
}

int	cell_map_opt()
{
	int	option=0;
        int V_clear(), V_line(), V_ques(), V_call(), G_warning();
	
	for(;;){
	V_clear();
	V_line(2,"   Select a cell map to display on the chosen window ");
	V_line(4,"   1. Erosion map"); 
	V_line(5,"   2. Deposition map");
	V_line(6,"   3. Sediment leaving the cell map ");
	V_line(7,"   4. Runoff from upstream map ");
	V_line(8,"   5. Runoff generated in each cell map ");
	V_line(9,"   6. Runoff to downstream map");
	V_line(10,"   7. Total N in Sediment generated map");
	V_line(11,"   8. Total N in Sediment leaving map");
	V_line(12,"   9. Total N in Runoff generated map");
	V_line(13,"   10. Total N in Runoff leaving map");
	V_line(14,"   11. Total P in Sediment generated map");
	V_line(15,"   12. Total P in Sediment leaving map");
	V_line(16,"   13. Total P in Runoff generated map");
	V_line(17,"   14. Total P in Runoff leaving map");
	V_line(18,"   15. Total COD in Runoff generated map");
	V_line(19,"   16. Total COD in Runoff leaving map");
	V_line(20,"   17. Others");
	V_line(21,"   Enter the choice (1-17):");
	V_ques(&option,'i',21,40,2);

	if(!V_call()) exit(1);
	
	if(option > 0 && option <=17) break;
	else G_warning("Please enter one of the choices between 1-17");
	}
	return(option);
}


int	display_where()
{
	int	option=0;
        int V_clear(), V_line(), V_ques(), V_call(), G_warning();

	for(;;){
	V_clear();
	V_line(3, "         Would you like to display on an existing window? ");
	V_line(7,"	    1. Yes"); 
	V_line(9,"	    2. No");
	V_line(11,"	    Enter the choice (1-2):");
	V_ques(&option,'i',11,40,1);

	if(!V_call()) exit(1);

	if(option > 0 && option <=2) break;
	else G_warning("Please enter one of the choices between 1-2");
	}
	
	return(option);
}

void	working_sngl()
{
	int G_clear_screen();
	G_clear_screen();

	printf("\n\nBusy !! \n\n");

}

int	save_opt_menu()
{
	int	option = 0;
        int V_clear(), V_line(), V_ques(), V_call(), G_warning();


	for(;;){
	V_clear();
	V_line(1, "         Save Option Menu");
	V_line(3,"	    1. Soil Loss (graphics)");
	V_line(5,"	    2. Nutrients Movement (graphics)");
	V_line(7,"	    3. Feedlot Analysis (graphics)");
	V_line(9,"	    4. Runoff Movement (graphics)");
	V_line(11,"	    5. Quit");
	V_line(13,"	    Enter the choice (1-5):");
	V_ques(&option,'i',13,40,1);

	if(!V_call()) exit(1);

	if(option > 0 && option <=5) break;

	if(!V_call()) exit(1);

	if(option > 0 && option <=5) break;
	else G_warning("Please enter one of the choices between 1-5");
	}

	if(option == 1) return(soil_loss);
	else if(option == 2) return(nutrients);
	else if(option == 3) return(feedlot);
	else if(option == 4) return(runoff);
	else return(0);

}


int	analyze_menu()
{
	int	option=8;
        int V_clear(), V_line(), V_ques(), V_call(), G_warning();
	
	for(;;){
	V_clear();
	V_line(1,"   Analyze Different Scenarios input and output Options");
	V_line(3,"   1. Zoom"); 
	V_line(4,"   2. View a Cell");
	V_line(5,"   3. View an area output");
	V_line(6,"   4. Toggle between flow direction map/viewing area");
	V_line(7,"   5. Show the watershed summary output");
	V_line(8,"   6. Displayed range of output maps ");
	V_line(9,"   7. Display user's choice map");
	V_line(10,"   8. Draw Cumulative and Frequency Distribution Stats");
	V_line(11,"   9. Restore the initial screen");
	V_line(12,"   10. Exit to GRASS (to come back type 'return') ");
	V_line(13,"   11. Analyze another output ");
	V_line(14,"   12. Quit");
	V_line(15,"   Enter the choice (1-12):");
	V_ques(&option,'i',15,40,2);

	if(!V_call()) exit(1);
	
	if(option > 0 && option <=12) break;
	else G_warning("Please enter one of the choices between 1-12");
	}
	return(option);

}

int	analyze_data_opt()
{
	int	option=0;
        int V_clear(), V_line(), V_ques(), V_call(), G_warning();

	for(;;){
	V_clear();
	V_line(3, "         Which data would you like to display");
	V_line(7,"	    1. Input"); 
	V_line(9,"	    2. Output"); 
	V_line(11,"	    Enter the choice (1-2):");
	V_ques(&option,'i',11,40,1);

	if(!V_call()) exit(1);
	
	if(option > 0 && option <=2) break;
	else G_warning("Please enter one of the choices between 1-2");
	}
	return(option);

}

int	analyze_data_run()
{
	int	option=0;
        int V_clear(), V_line(), V_ques(), V_call(), G_warning();

	for(;;){
	V_clear();
	V_line(3, "         Which data would you like to display");
	V_line(7,"	    1. Current"); 
	V_line(9,"	    2. Selected"); 
	V_line(11,"	    Enter the choice (1-2):");
	V_ques(&option,'i',11,40,1);

	if(!V_call()) exit(1);
	
	if(option > 0 && option <=2) break;
	else G_warning("Please enter one of the choices between 1-2");
	}
	return(option);
}

int	analyze_output_sel()
{
	int	option=0;
        int V_clear(), V_line(), V_ques(), V_call(), G_warning();

	for(;;){
	V_clear();
	V_line(3, "         Which output data would you like to analyze");
	V_line(7,"	    1. Sediment Movement"); 
	V_line(9,"	    2. Runoff"); 
	V_line(11,"	    3. N attached to Sediment"); 
	V_line(13,"	    4. N attached to Runoff"); 
	V_line(15,"	    5. P attached to Sediment"); 
	V_line(17,"	    6. P attached to Runoff"); 
	V_line(19,"	    7. COD "); 
	V_line(21,"	    Enter the choice (1-7):");
	V_ques(&option,'i',21,40,1);

	if(!V_call()) exit(1);
	
	if(option > 0 && option <=7) break;
	else G_warning("Please enter one of the choices between 1-7");
	}

	if(option == 1){
		SED_ANALYSIS = YES;
		RO_ANALYSIS  = NO;
		N_SED_ANALYSIS = NO;
		P_SED_ANALYSIS = NO;
		N_RO_ANALYSIS = NO;
		P_RO_ANALYSIS = NO;
		COD_ANALYSIS = NO;
		return(soil_loss);
		}
	else if(option == 2){
		SED_ANALYSIS = NO;
		RO_ANALYSIS  = YES;
		N_SED_ANALYSIS = NO;
		P_SED_ANALYSIS = NO;
		N_RO_ANALYSIS = NO;
		P_RO_ANALYSIS = NO;
		COD_ANALYSIS = NO;
		return(runoff);
		}
	else if(option == 3){
		SED_ANALYSIS = NO;
		RO_ANALYSIS  = NO;
		N_SED_ANALYSIS = YES;
		P_SED_ANALYSIS = NO;
		N_RO_ANALYSIS = NO;
		P_RO_ANALYSIS = NO; 
		COD_ANALYSIS = NO;
		NUTRIENT = N;
		NUT_ATTCH = sed;
		return(nutrients);
		}
	else if(option == 4){
		SED_ANALYSIS = NO;
		RO_ANALYSIS  = NO;
		N_SED_ANALYSIS = NO;
		P_SED_ANALYSIS = NO;
		N_RO_ANALYSIS = YES;
		P_RO_ANALYSIS = NO; 
		COD_ANALYSIS = NO;
		NUTRIENT = N;
		NUT_ATTCH = ro;
		return(nutrients);
		}
	else if(option == 5){
		SED_ANALYSIS = NO;
		RO_ANALYSIS  = NO;
		N_SED_ANALYSIS = NO;
		P_SED_ANALYSIS = YES;
		N_RO_ANALYSIS = NO;
		P_RO_ANALYSIS = NO; 
		COD_ANALYSIS = NO;
		NUTRIENT = P;
		NUT_ATTCH = sed;
		return(nutrients);
		}
	else if(option == 6){
		SED_ANALYSIS = NO;
		RO_ANALYSIS  = NO;
		N_SED_ANALYSIS = NO;
		P_SED_ANALYSIS = NO;
		N_RO_ANALYSIS = NO;
		P_RO_ANALYSIS = YES; 
		COD_ANALYSIS = NO;
		NUTRIENT = P;
		NUT_ATTCH = ro;
		return(nutrients);
		}
	else if(option == 7){
		SED_ANALYSIS = NO;
		RO_ANALYSIS  = NO;
		N_SED_ANALYSIS = NO;
		P_SED_ANALYSIS = NO;
		N_RO_ANALYSIS = NO;
		P_RO_ANALYSIS = NO; 
		COD_ANALYSIS = YES;
		NUTRIENT = COD;
		return(nutrients);
		}
        return 0;
}

int	cdf_var_opt()
{
	int	option=0;
        int V_clear(), V_line(), V_ques(), V_call(), G_warning();
	
	for(;;){
	V_clear();
	V_line(1,"Select a variable to draw frequency distribution curve");
	V_line(2,"   Output Variables				Input Vaiables");
	V_line(4,"   1. Erosion					17. Curve Number"); 
	V_line(5,"   2. Deposition				18. Slope ");
	V_line(6,"   3. Sediment leaving the cell 		19. Slope Length");
	V_line(7,"   4. Runoff from upstream			20. USLE K factor");
	V_line(8,"   5. Runoff generated in each cell 		21. USLE C factor");
	V_line(9,"   6. Runoff to downstream 			22. Fertilizar Factor");
	V_line(10,"   7. Total N in Sediment generated ");
	V_line(11,"   8. Total N in Sediment leaving ");
	V_line(12,"   9. Total N in Runoff generated ");
	V_line(13,"   10. Total N in Runoff leaving ");
	V_line(14,"   11. Total P in Sediment generated ");
	V_line(15,"   12. Total P in Sediment leaving ");
	V_line(16,"   13. Total P in Runoff generated ");
	V_line(17,"   14. Total P in Runoff leaving ");
	V_line(18,"   15. Total COD in Runoff generated ");
	V_line(19,"   16. Total COD in Runoff leaving ");
	V_line(21,"   Enter the choice (1-22):");
	V_ques(&option,'i',21,40,2);

	if(!V_call()) exit(1);
	
	if(option > 0 && option <=22) break;
	else G_warning("Please enter one of the choices between 1-22");
	}

	if(option == 17) return(CN);
	else if(option == 18) return(Slope);
	else if(option == 19) return(Slope_ln);
	else if(option == 20) return(K_val);
	else if(option == 21) return(C_fac);
	else if(option == 22) return(Fert_fac);
	else return(option);
}

int	cdf_incr_opt()
{
	int	option=0;
        int V_clear(), V_line(), V_ques(), V_call(), G_warning();

	for(;;){
	V_clear();
	V_line(3, "         Would you like to choose the selected variable intervals?"); 
	V_line(7,"	    1. Yes"); 
	V_line(9,"	    2. No ");
	V_line(10,"	    (10 equal intervals will be selected between max and min)"); 
	V_line(12,"	    Enter the choice (1-2):");
	V_ques(&option,'i',12,40,1);

	if(!V_call()) exit(1);
	
	if(option > 0 && option <=2) break;
	else G_warning("Please enter one of the choices between 1-2");
	}
	return(option);
}
