
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
	
	drain_num()

	To generate a map showing the number of the drain cell to which the
	corresponding cell drains. This is achived using the cell number
	map and the aspect map from elevation layer. A temp map
	of temp_drain is created and saved depending on users choice.

	Also, number of cells accumulated in each cell is estimated.
*/

#include "agnps_input.h"


drain_num()
{

	int	i, j, ct, k;
	int	nrow, ncol;

	CELL *elev_cell[3], *temp;

	G_set_window(&orig_window);

	nrow = orig_window.rows;
	ncol = orig_window.cols;

	elev_cell[0] = G_allocate_cell_buf();
	elev_cell[1] = G_allocate_cell_buf();
	elev_cell[2] = G_allocate_cell_buf();

/* allocate memory for drain map and its name */
	temp_drain_map = (MAPS *) emalloc ((unsigned) sizeof (MAPS));
	temp_drain_map->p = emalloc ((unsigned) (64));
	
	strcpy(temp_drain_map->p, "temp_drain");

	temp_drain_map->fd = cell_open_new(temp_drain_map->p);

	temp_drain_map->rbuf = G_allocate_cell_buf();

	cell_num_map->fd = cell_open(cell_num_map->p,this_mapset);

	ct = 1;

	for(i = 0; i < nrow; i++)
	{
	if(i != 0)
	   G_get_map_row_nomask (cell_num_map->fd, elev_cell[1], i-1);
	   G_get_map_row_nomask (cell_num_map->fd, elev_cell[2], i);

	   temp = elev_cell[0];
	   elev_cell[0] = elev_cell[1];
	   elev_cell[1] = elev_cell[2];
	if(i != nrow-1)
	   G_get_map_row_nomask (cell_num_map->fd, elev_cell[2] = temp, i+1);

	   G_get_map_row_nomask (temp_dir_map->fd, temp_dir_map->rbuf, i);
	   G_get_map_row_nomask (wshd->fd, wshd->rbuf, i);

	   G_zero_cell_buf(temp_drain_map->rbuf);

	   for(j=0;j < ncol;j++){
	      if(wshd->rbuf[j] > 0){
	        switch(temp_dir_map->rbuf[j]){
		      
		      case 1:
			  temp_drain_map->rbuf[j] = elev_cell[0][j];
			  break;
		      case 2:
			  temp_drain_map->rbuf[j] = elev_cell[0][j+1];
			  break;
		      case 3:
			  temp_drain_map->rbuf[j] = elev_cell[1][j+1];
			  break;
		      case 4:
			  temp_drain_map->rbuf[j] = elev_cell[2][j+1];
			  break;
		      case 5:
			  temp_drain_map->rbuf[j] = elev_cell[2][j];
			  break;
		      case 6:
			  temp_drain_map->rbuf[j] = elev_cell[2][j-1];
			  break;
		      case 7:
			  temp_drain_map->rbuf[j] = elev_cell[1][j-1];
			  break;
		      case 8:
			  temp_drain_map->rbuf[j] = elev_cell[0][j-1];
			  break;
		      default :
			  temp_drain_map->rbuf[j] = 0;
			  break;

	   		}
			cel[ct].rcell_num = temp_drain_map->rbuf[j];
			ct++;
	          }
	   }

	   G_put_map_row(temp_drain_map->fd,temp_drain_map->rbuf);
	}


	G_close_cell(temp_drain_map->fd);
	G_close_cell(temp_dir_map->fd);
	G_close_cell(cell_num_map->fd);

}
