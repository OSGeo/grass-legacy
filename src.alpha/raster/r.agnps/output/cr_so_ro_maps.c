
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

int cr_so_ro_maps(j)
int	j;
{
	char	buf[512]; 
	char	*emalloc();
	struct	Colors	sed_in_colors, sed_gen_colors, sed_out_colors;
        int make_grn_yel_red();
        int G_init_colors(), G_set_color(), G_write_colors(), G_free_colors();


	printf("Creating Color structures for Sediment movement maps\n");

/* initiate the colors structures for the maps */

	G_init_colors(&sed_in_colors);
	G_init_colors(&sed_gen_colors);
	G_init_colors(&sed_out_colors);


	make_grn_yel_red(&sed_in_colors,(int)(gen_above_min[j]*sig_fac), (int) (gen_above_max[j]*sig_fac));
	make_grn_yel_red(&sed_out_colors,(int)(yield_min[j]*sig_fac), (int) (yield_max[j]*sig_fac));
	make_grn_yel_red(&sed_gen_colors,(int)(within_min[j]*sig_fac), (int) (within_max[j]*sig_fac));

	G_set_color((CELL)0,0,0,0,&sed_out_colors);
	G_set_color((CELL)0,0,0,0,&sed_gen_colors);
	G_set_color((CELL)0,0,0,0,&sed_in_colors);

	if(j == cur){
	G_write_colors(sed_in,this_mapset,&sed_in_colors);
	G_write_colors(sed_gen,this_mapset,&sed_gen_colors);
	G_write_colors(sed_out,this_mapset,&sed_out_colors);
	}
	if(j == prev){
	sprintf(buf,"%s_prev",sed_in);
	G_write_colors(buf,this_mapset,&sed_in_colors);
	sprintf(buf,"%s_prev",sed_gen);
	G_write_colors(buf,this_mapset,&sed_gen_colors);
	sprintf(buf,"%s_prev",sed_out);
	G_write_colors(buf,this_mapset,&sed_out_colors);
	}
		
	G_free_colors(&sed_in_colors);
	G_free_colors(&sed_gen_colors);
	G_free_colors(&sed_out_colors);

	printf("Creating Color structures for Runoff movement maps\n");

/* initiate the colors structures for the maps */

	G_init_colors(&sed_in_colors);
	G_init_colors(&sed_gen_colors);
	G_init_colors(&sed_out_colors);

	make_grn_yel_red(&sed_in_colors,(int)(ro_us_min[j]*sig_fac), (int) (ro_us_max[j]*sig_fac));
	make_grn_yel_red(&sed_out_colors,(int)(ro_ds_min[j]*sig_fac), (int) (ro_ds_max[j]*sig_fac));
	make_grn_yel_red(&sed_gen_colors,(int)(ro_gen_min[j]*sig_fac), (int)(ro_gen_max[j]*sig_fac));

	G_set_color((CELL)0,0,0,0,&sed_out_colors);
	G_set_color((CELL)0,0,0,0,&sed_gen_colors);
	G_set_color((CELL)0,0,0,0,&sed_in_colors);

	if(j == cur){
	G_write_colors(ro_us,this_mapset,&sed_in_colors);
	G_write_colors(ro_gen,this_mapset,&sed_gen_colors);
	G_write_colors(ro_ds,this_mapset,&sed_out_colors);
	}
	if(j == prev){
	sprintf(buf,"%s_prev",ro_us);
	G_write_colors(buf,this_mapset,&sed_in_colors);
	sprintf(buf,"%s_prev",ro_gen);
	G_write_colors(buf,this_mapset,&sed_gen_colors);
	sprintf(buf,"%s_prev",ro_ds);
	G_write_colors(buf,this_mapset,&sed_out_colors);
	}
		
	G_free_colors(&sed_in_colors);
	G_free_colors(&sed_gen_colors);
	G_free_colors(&sed_out_colors);
        return 0;
}

char *emalloc (n)
unsigned n;
{
    char *p;

    if ((p = (char *)G_malloc (n*sizeof(char))) == NULL) {
    	exit (1);
	}
	return (p);
}

