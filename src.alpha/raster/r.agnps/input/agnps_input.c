
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

#include "agnps_input.h"
#include <stdlib.h>
#pragma ident "r.agnps.input v 1.1B <25 Feb 1995>; Copyright (c) 1992-95. Purdue Research Foundation, West   Lafayette, Indiana 47907. All Rights Reserved.";

/*      July, 1991  Agricultural Engineering, Purdue University
	Raghavan Srinivasan (srin@ecn.purdue.edu)

	main()

	This is the main program to acquire the needed inputs from the user
	and process them for the format needed by the AGNPS model.
	Once the map names are gathered the processes are started to
	manipulate them and temp maps are created. Here by masking the
	wshd outline the data are gathered and put into AGNPS format.
*/

int main (argc, argv)
  char **argv;
  int argc;
{
	int	i, j, nrows, ncols, ct, land_use;
	int	clay1, silt1, sand1;
	FILE	*fopen(), *fs;
	float	rules_ch_side_slope();
	float	rules_man_n();
	float	rules_sur_cond();
	char	buf[512];
	double	L, S, LS;
	double 	m; /* exponential factor in LS factor equation */
	double	theta; /* angle of slope converted from % of slope */
	struct Categories landuse_cats, mgt_practice_cats;
	struct Categories hy_cond_cats;
	struct Categories machinery_cats, K_fac_cats, C_fac_cats;
	struct Categories clay_cats, sand_cats;
        void clean_up();
        int get_wshd_input(), get_input_map_names(), cell_num_id();
        int slope_aspect(), drain_num(), chkdata(), CN_hy_cond();
        int op_cel_fls(), rules_cod(), rules_fert_aval(), rules_soil_texture();
       int G_gisinit(), G_read_cats(), G_get_set_window();
       int G_get_map_row(), G_fatal_error(), G_close_cell();

	get_wshd_input();

       G_gisinit (argv[0]);

	this_mapset = G_mapset();

	get_input_map_names();

	cell_num_id();
	 
	slope_aspect();

	drain_num();

/* check for any error in the aspect maps */
	chkdata();


/* generate CN, and hy_cond maps */

	CN_hy_cond();

/* open the cell files created for reading (slope, aspect, rcell) */

	op_cel_fls();

/* get the category names and cell title */

	if (G_read_cats (machinery->p, machinery->mapset, &machinery_cats) < 0){
		clean_up();
		exit(-1);
		}
		/*
	if (G_read_cats (nutrient->p, nutrient->mapset, &nutrient_cats) < 0)
		exit(-1);
	if (channel_slope->flag == YES){
	if (G_read_cats (channel_slope->p, channel_slope->mapset, &channel_slope_cats) < 0)
		clean_up();
		exit(-1);
		}
		*/
	if (G_read_cats (landuse->p, landuse->mapset, &landuse_cats) < 0){
		clean_up();
		exit(-1);
		}
	if (G_read_cats (mgt_practice->p, mgt_practice->mapset, &mgt_practice_cats) < 0){
		clean_up();
		exit(-1);
		}
	if (G_read_cats (hy_cond->p, this_mapset, &hy_cond_cats) < 0){
		clean_up();
		exit(-1);
		}
	if (G_read_cats (C_fac->p, C_fac->mapset, &C_fac_cats) < 0){
		clean_up();
		exit(-1);
		}
	if (G_read_cats (K_fac->p, K_fac->mapset, &K_fac_cats) < 0){
		clean_up();
		exit(-1);
		}
	if (G_read_cats (sand->p, sand->mapset, &sand_cats) < 0){
		clean_up();
		exit(-1);
		}
	if (G_read_cats (clay->p, clay->mapset, &clay_cats) < 0){
		clean_up();
		exit(-1);
		}



	G_get_set_window (&orig_window);
	nrows = orig_window.rows;
	ncols = orig_window.cols;


	if ((fs = fopen (in_fl_name,"w")) == NULL)  {
		printf ("Input file for AGNPS model can't open  \n");
		clean_up();
		exit(0);
		}


/* write project description and other wshd parameters in the data file */	

	fprintf(fs,"%s\n%4.1f%4d%6.1f%6.1f%6d\n",wshd_des,cell_area,tot_cells,rainfall,ei,1);

	ct = 1;
	for(i = 0; i < nrows; i++) {

	G_get_map_row(wshd->fd,wshd->rbuf,i);
	G_get_map_row(temp_slope_map->fd,temp_slope_map->rbuf,i);
	G_get_map_row(temp_dir_map->fd,temp_dir_map->rbuf,i);
	G_get_map_row(temp_drain_map->fd,temp_drain_map->rbuf,i);
	G_get_map_row(nutrient->fd,nutrient->rbuf,i);
	G_get_map_row(machinery->fd,machinery->rbuf,i);
	if (channel_slope->flag == YES)
	G_get_map_row(channel_slope->fd,channel_slope->rbuf,i);
	G_get_map_row(landuse->fd,landuse->rbuf,i);
	G_get_map_row(mgt_practice->fd,mgt_practice->rbuf,i);
	G_get_map_row(K_fac->fd,K_fac->rbuf,i);
	G_get_map_row(C_fac->fd,C_fac->rbuf,i);
	G_get_map_row(sand->fd,sand->rbuf,i);
	G_get_map_row(clay->fd,clay->rbuf,i);
	G_get_map_row(hy_cond->fd,hy_cond->rbuf,i);
	G_get_map_row(temp_cn_map->fd,temp_cn_map->rbuf,i);

	   for(j=0;j < ncols;j++) {
	   	if(wshd->rbuf[j] > 0){
		  land_use = assign_landuse(G_tolcase(G_get_cat(landuse->rbuf[j], &landuse_cats)));

/* check for landuse for slope, also check for 0 slope */
		  if(land_use == water || land_use == marsh)
		     cel[ct].ovl_slope = 0.0;
		  else if (temp_slope_map->rbuf[j] == 0)
		     cel[ct].ovl_slope = 0.5;
		  else
		     cel[ct].ovl_slope = (float) temp_slope_map->rbuf[j]/10.0;

		  cel[ct].CN = temp_cn_map->rbuf[j];

/* assume an uniform land shape since after smoothing it would be uniform */
		  cel[ct].slp_shpe_fact = 1;

/* LS factor is estimated using I.D. Moore paper */
		  theta = (double)(atan((double)(cel[ct].ovl_slope/100.0)));
		  L = pow(((double)((cel[ct].acc+1)*grid_res)/22.13), 0.4); /* L = accumulation_cells * grid_res/22.13 to the power 0.4 */
		  S = pow((sin(theta)/0.0896),1.3); /* convert pecentage of slope to degrees and take sin of this then divide by 0.0896 and raise to the power 1.3 */
		  LS = L*S; /* LS factor */
/* convert from LS factor to field slope length using 537 manual */
		  if(cel[ct].ovl_slope < 1.0) m = 0.2;
		  else if(cel[ct].ovl_slope < 3.0) m = 0.3;
		  else if(cel[ct].ovl_slope < 4.5) m = 0.4;
		  else if(cel[ct].ovl_slope < 5.0) m = 0.5;

		  cel[ct].fld_slp_len = (int) (pow((LS/((double)((65.41*sin(theta)*sin(theta))+4.56*sin(theta)+0.065))),(1/m))*72.6);

		  if(cel[ct].fld_slp_len > (int) (grid_res*3.28)) cel[ct].fld_slp_len = (int) (grid_res*3.28);

/* After the discussion with Dr. Engel, the slope length factor normally will
not vary more than 400 feet, so limit of 400 feet is imposed if the estimated
slope length exceeds */
		  if(cel[ct].fld_slp_len > 400) cel[ct].fld_slp_len = 400;
		  if(cel[ct].fld_slp_len == 0) cel[ct].fld_slp_len = 1;


		  cel[ct].fert_level = nutrient->rbuf[j];

		  cel[ct].aspect = temp_dir_map->rbuf[j];

/* assume the P-factor as 1.0 which is the worse case and for row crops */
		  cel[ct].P_fac = 1.0;

/* get USLE C and K facators from respective maps */
		  cel[ct].K_fac = (float) atof(G_get_cat(K_fac->rbuf[j],&K_fac_cats));
/* check for USLE K value between 0-1 else report error and exit */
		if(cel[ct].K_fac < 0.0 || cel[ct].K_fac >1.0){
		     sprintf(buf, "USLE K factor for soil category number [%d] not in allowable range of 0-1\n",K_fac->rbuf[j]);
		     G_fatal_error(buf);
		     clean_up();
		     exit(1);
		     }

		  cel[ct].C_fac = (float) atof(G_get_cat(C_fac->rbuf[j],&C_fac_cats));

/* check for USLE C value between 0-1 else report error and exit */
		if(cel[ct].C_fac < 0.0 || cel[ct].C_fac >1.0){
			sprintf(buf, "USLE C factor for landuse category number [%d] not in allowable range of 0-1\n",C_fac->rbuf[j]);
			G_fatal_error(buf);
			clean_up();
			exit(1);
			}

/* assume pt_src, gully_src, impd_fac and chl_ind are not exist, so assign 0 */

		  cel[ct].pt_src = 0;
		  cel[ct].gully_src = 0;
		  cel[ct].impd_fac = 0;

/* get clay, sand, silt values */
		  clay1 = atoi(G_get_cat(clay->rbuf[j],&clay_cats));
		  sand1 = atoi(G_get_cat(sand->rbuf[j],&sand_cats));
		  silt1 = 100 - (clay1+sand1);

/* assume 50% of overland slope as channel slope if the channel
   slope map is not exsist */

		  if (channel_slope->flag == YES){
			cel[ct].chl_slope = (float) channel_slope->rbuf[j]/10;
			if(cel[ct].chl_slope > 0.0) cel[ct].chl_indicator = 1;
			if (cel[ct].chl_slope == 0.0) cel[ct].ch_side_slope = 0.0;
			else cel[ct].ch_side_slope = rules_ch_side_slope(clay1, silt1, sand1, land_use);
			}
		else {
			cel[ct].chl_slope = (float) 0.5 * cel[ct].ovl_slope;
			cel[ct].chl_indicator = 0;
			cel[ct].ch_side_slope = rules_ch_side_slope(clay1, silt1, sand1, land_use);
			}

		  cel[ct].COD_fac = rules_cod(land_use);
		  cel[ct].incor_level = rules_fert_aval(G_tolcase(G_get_cat(machinery->rbuf[j], &machinery_cats)),land_use);
		  cel[ct].man_n = rules_man_n(land_use);
		  cel[ct].texture = rules_soil_texture(clay1,silt1,sand1);
		  cel[ct].sur_const = rules_sur_cond(land_use, G_tolcase(G_get_cat(mgt_practice->rbuf[j], &mgt_practice_cats)),G_tolcase(G_get_cat(hy_cond->rbuf[j], &hy_cond_cats)));
		  ct++;
		  }
	      }
	 }


	 G_close_cell(temp_drain_map->fd);
	 G_close_cell(temp_slope_map->fd);
	 G_close_cell(temp_dir_map->fd);
	 G_close_cell(nutrient->fd);
	 if (channel_slope->flag == YES)
	 	G_close_cell(channel_slope->fd);
	 G_close_cell(landuse->fd);
	 G_close_cell(mgt_practice->fd);
	 G_close_cell(machinery->fd);
	 G_close_cell(K_fac->fd);
	 G_close_cell(C_fac->fd);
	 G_close_cell(hy_cond->fd);
	 G_close_cell(wshd->fd);

	 for(i=1; i <= tot_cells; i++){
	    fprintf(fs,"%7d%7d%4d%5.1f%2d%4d%5.1f%5.1f%5.3f%4.2f%4.2f%5.2f%4.2f%2d%2d%2d%4d%2d%4d%4d%3d%2d\n",
		    cel[i].cell_num*1000,cel[i].rcell_num*1000,cel[i].CN,
		    cel[i].ovl_slope,cel[i].slp_shpe_fact, cel[i].fld_slp_len,
		    cel[i].chl_slope,cel[i].ch_side_slope, cel[i].man_n,
		    cel[i].K_fac,cel[i].C_fac, cel[i].P_fac,
		    cel[i].sur_const,cel[i].aspect, cel[i].texture,
		    cel[i].fert_level,cel[i].incor_level, cel[i].pt_src,
		    cel[i].gully_src,cel[i].COD_fac, cel[i].impd_fac,
		    cel[i].chl_indicator);
		}

	 fclose(fs);



	clean_up();
        return 0;
}
