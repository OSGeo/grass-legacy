
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

int read_input(name,j)
char	*name;
int	j;  /* read current or previous run data sets */
{
	int	i;
	char    buf[1024], *buf1, *stringncpy();
	FILE	*fs;
int G_remove(), G_warning(), G_fatal_error();

	sprintf(buf,"%s.dat",name);
	if ((fs = fopen(buf,"r")) == NULL && j != prev){
		printf("ASCII AGNPS Input file %s can't be open\n",buf);
		G_remove("cell",cell_num_map->p);
		exit(0);
		}
	else if ((fs = fopen(buf,"r")) == NULL && j == prev){
		ANALYSIS = NO;
		G_warning("The ASCII file not found to read the input");
		return 0;
		}

	printf("Reading AGNPS ASCII input file\n");

	fgets(buf,1024,fs);

	/*
	fgets(buf,1024,fs);

	sscanf(buf,"%f %d %f %f %d",&cell_area[j],&no_cells[j],&rainfall[j],&ei[j],&i);


	fscanf(fs,"%4.1f",&cell_area[j]);
	fscanf(fs,"%4d%6.1f%6.1f%6d",&no_cells,&rainfall,&ei,&i);
	*/

	fgets(buf,1024,fs);

	buf1 = stringncpy(buf,0,4);
	sscanf(buf1,"%f",&cell_area[j]);

	buf1 = stringncpy(buf,4,4);
	sscanf(buf1,"%d",&no_cells[j]);

	if( no_cell != no_cells[j]) {
		fclose(fs);
		G_fatal_error("The number of cells in the AGNPS ASCII input file and\n the watershed map is not matching.\n Either one of them is wrong!!!!");
		}

	if(j == prev && no_cells[cur] != no_cells[prev]) {
		ANALYSIS = NO;
		G_warning("The number of cells is not matching with currect data");
		fclose(fs);
		return 0;
		}

	buf1 = stringncpy(buf,8,6);
	sscanf(buf1,"%f",&rainfall[j]);

	buf1 = stringncpy(buf,14,6);
	sscanf(buf1,"%f",&ei[j]);

	for(i = 0; i < no_cells[j]; i++){

	/*
	   fscanf(fs,"%7d%7d%4d%5.1f%2d%4d%5.1f%5.1f%5.3f%4.2f%4.2f%5.2f%4.2f%2d%2d%2d%4d%2d%4d%4d%3d%2d",&ag_inp[i].cell_num,&ag_inp[i].rcell_num,&ag_inp[i].cn,&ag_inp[i].slope_pct,&ag_inp[i].slope_shape,&ag_inp[i].slope_ln,&ag_inp[i].chnl_slope,&ag_inp[i].chnl_side_slope,&ag_inp[i].man_n,&ag_inp[i].k_val,&ag_inp[i].c_fac,&ag_inp[i].p_fac,&ag_inp[i].surf_cond,&ag_inp[i].aspect,&ag_inp[i].texture,&ag_inp[i].fert_fac,&ag_inp[i].fert_avl_fac,&ag_inp[i].feedlot,&ag_inp[i].gully,&ag_inp[i].cod_fac,&ag_inp[i].impd_fac,&ag_inp[i].chnl_ind);

	*/

	fgets(buf,1024,fs);

	buf1 = stringncpy(buf,0,7);
	sscanf(buf1,"%d",&ag_inp[j][i].cell_num);

	buf1 = stringncpy(buf,7,7);
	sscanf(buf1,"%d",&ag_inp[j][i].rcell_num);

	buf1 = stringncpy(buf,14,4);
	sscanf(buf1,"%d",&ag_inp[j][i].cn);

	buf1 = stringncpy(buf,18,5);
	sscanf(buf1,"%f",&ag_inp[j][i].slope_pct);

	buf1 = stringncpy(buf,23,2);
	sscanf(buf1,"%d",&ag_inp[j][i].slope_shape);

	buf1 = stringncpy(buf,25,4);
	sscanf(buf1,"%d",&ag_inp[j][i].slope_ln);

	buf1 = stringncpy(buf,29,5);
	sscanf(buf1,"%f",&ag_inp[j][i].chnl_slope);

	buf1 = stringncpy(buf,34,5);
	sscanf(buf1,"%f",&ag_inp[j][i].chnl_side_slope);

	buf1 = stringncpy(buf,39,5);
	sscanf(buf1,"%f",&ag_inp[j][i].man_n);

	buf1 = stringncpy(buf,44,4);
	sscanf(buf1,"%f",&ag_inp[j][i].k_val);

	buf1 = stringncpy(buf,48,4);
	sscanf(buf1,"%f",&ag_inp[j][i].c_fac);

	buf1 = stringncpy(buf,52,5);
	sscanf(buf1,"%f",&ag_inp[j][i].p_fac);

	buf1 = stringncpy(buf,57,4);
	sscanf(buf1,"%f",&ag_inp[j][i].surf_cond);

	buf1 = stringncpy(buf,61,2);
	sscanf(buf1,"%d",&ag_inp[j][i].aspect);

	buf1 = stringncpy(buf,63,2);
	sscanf(buf1,"%d",&ag_inp[j][i].texture);

	buf1 = stringncpy(buf,65,2);
	sscanf(buf1,"%d",&ag_inp[j][i].fert_fac);

	buf1 = stringncpy(buf,67,4);
	sscanf(buf1,"%d",&ag_inp[j][i].fert_avl_fac);

	buf1 = stringncpy(buf,71,2);
	sscanf(buf1,"%d",&ag_inp[j][i].feedlot);

	buf1 = stringncpy(buf,73,4);
	sscanf(buf1,"%d",&ag_inp[j][i].gully);

	buf1 = stringncpy(buf,77,4);
	sscanf(buf1,"%d",&ag_inp[j][i].cod_fac);

	buf1 = stringncpy(buf,81,3);
	sscanf(buf1,"%d",&ag_inp[j][i].impd_fac);

	buf1 = stringncpy(buf,84,2);
	sscanf(buf1,"%d",&ag_inp[j][i].chnl_ind);

	   }

	   fclose(fs);
   return 0;
}


char *stringncpy(s,start,n)
char s[];
int   start,n;
{

	int i;
	char str[512];

	for (i = 0; i <512 ; i++) str[i] = '\0';

	for (i = 0; i <n ; i++) str[i] = s[start+i];
	return(str);
	
}
