
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

/*	July, 1991  Agricultural Engineering, Purdue University
	Raghavan Srinivasan (srin@ecn.purdue.edu)
	
	slope_aspect()

	To generate a slope and aspect maps from the given elevation map 
	using the neibourhood techinique for slope prediction.
	And it creates a new temp map called temp_slope which has the values
	of slope in tenths of percent and also creates an aspect map according to
	the AGNPS program format starting 1 as North and goes clockwise
	direction. This newly created map will be saved later by user chosen 
	name, so that this could be used for editing any maps that may have 
	any error found by the chkdata routines.
*/

#include "agnps_input.h"


slope_aspect()
{

	int	i, j;
	int	nrow, ncol;

	CELL *elev_cell[3], *temp;
	long int c1, c2, c3, c4, c5, c6, c7, c8;
	double p,q;
	double H,V;
	double key;

	G_set_window(&orig_window);

	nrow = orig_window.rows;
	ncol = orig_window.cols;

	elev_cell[0] = G_allocate_cell_buf();
	elev_cell[1] = G_allocate_cell_buf();
	elev_cell[2] = G_allocate_cell_buf();

	H = orig_window.ew_res * 4 * 2;  /* horizontal (east-west) run times 4 for weighted difference */
	V = orig_window.ns_res * 4 * 2;


	
	strcpy(temp_slope_map->p, "temp_slope");


	temp_slope_map->fd = cell_open_new(temp_slope_map->p);

	temp_slope_map->rbuf = G_allocate_cell_buf();

	G_put_map_row(temp_slope_map->fd,temp_slope_map->rbuf);

	   G_get_map_row_nomask (elev->fd, elev_cell[1], 0);
	   G_get_map_row_nomask (elev->fd, elev_cell[2], 1);
	for(i = 2; i < nrow; i++)
	{

	   temp = elev_cell[0];
	   elev_cell[0] = elev_cell[1];
	   elev_cell[1] = elev_cell[2];
	   G_get_map_row_nomask (elev->fd, elev_cell[2] = temp, i);

	   G_zero_cell_buf(temp_slope_map->rbuf);

	   for(j=1;j < ncol-1;j++){
	   	c1 = elev_cell[0][j-1];
	   	c2 = elev_cell[0][j];
	   	c3 = elev_cell[0][j+1];
	   	c4 = elev_cell[1][j-1];
	   	c5 = elev_cell[1][j+1];
	   	c6 = elev_cell[2][j-1];
	   	c7 = elev_cell[2][j];
	   	c8 = elev_cell[2][j+1];
	    
	   	p = ((c1 + c4 + c4 + c6) - (c3 + c5 + c5 + c8)) / H;
	   	q = ((c6 + c7 + c7 + c8) - (c1 + c2 + c2 + c3)) / V;
	   
	   	key = sqrt(p*p + q*q);
		temp_slope_map->rbuf[j] = (int) (key*1000);
	   }

	   G_put_map_row(temp_slope_map->fd,temp_slope_map->rbuf);
	}


	G_close_cell(temp_slope_map->fd);
	G_close_cell(elev->fd);

/* To create the aspect map using the direct program */

	/* check the aspect direction of any error */
}
