
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

/*	June, 1991  Agricultural Engineering, Purdue University
	Raghavan Srinivasan (srin@ecn.purdue.edu)
	
	cell_num_id()

	To get the original window from the used supplied watershed map 
	and find the nrows, ncols and area of each cell. Also this routine
	creates a new temp map called temp_cell_num which has the values
	of cell number in an increasing order from left to right and
	from top to bottom as requested by the AGNPS and to find the
	total number of cells in the watershed. This newly created map
	will be saved later by user chosen name, so that this could be
	used for editing any maps that may have any error found by the
	chkdata routines.

	Also create a mask for the given watershed outline using reclass program
*/

#include "agnps_input.h"


cell_num_id()
{

	int	i, j, ct;
	int north_row, south_row, east_col, west_col;
	int	nrow, ncol;
	FILE	*fd, *fopen();
        FILE    *temp;
	char	buf[128], *tempfile;
	double north, south, east, west;


/* get the original window information  from the watershed cell header */
	fprintf (stderr,"Creating cell number map using the %s\n",wshd->p);

	G_get_cellhd(wshd->p,wshd->mapset,&orig_window);

/* if the user choose a grid size other than the original cell size of the watershed
  modify the window accordingly and set all the window parameters */

	fprintf (stderr,"The grid size selected by the user is %d meters \n",grid_res);

	if(grid_res != 0){
	   orig_window.ns_res = (double) grid_res;
	   orig_window.ew_res = (double) grid_res;
	   orig_window.rows = (int) ((orig_window.north - orig_window.south)/ orig_window.ns_res);
	   orig_window.cols = (int) ((orig_window.east - orig_window.west)/ orig_window.ew_res);
	   orig_window.south = orig_window.north - (orig_window.rows* orig_window.ns_res);
	   orig_window.west = orig_window.east - (orig_window.cols* orig_window.ew_res);
	   }

	G_set_window(&orig_window);
	G_put_window(&orig_window);

	nrow = orig_window.rows;
	ncol = orig_window.cols;
	cell_area = (float) orig_window.ns_res * orig_window.ew_res/4000.0; /* converts cell area from m**2 to acre */


	/* read the cell head of the raster layer given as the wshd mask layer,
	and start with that as a base. then read the cell values in the mask.
	all non-zero values are considered "in the mask". while reading the values,
	keep track of where the extreme n, s, e, and w cells are. then calculate
	the region n, s, e, and w limits, giving a 2 cell border (unless the
	original border created by the cellhd was less) around the "mask" area. */

	wshd->fd = cell_open(wshd->p,wshd->mapset);

	strcpy(cell_num_map->p, "temp_cell_num");
	cell_num_map->fd = cell_open_new(cell_num_map->p);
	cell_num_map->rbuf = G_allocate_cell_buf();

/* To create a temp file for reclassing the wshd boundry into MASK layer */

	tempfile = G_tempfile();
	fd = fopen(tempfile,"w");
        temp = fopen("cell.out","w");

	ct = 1;
		 
	for(i = 0; i < nrow; i++)
	{
	   G_get_map_row(wshd->fd,wshd->rbuf,i);
	   G_zero_cell_buf(cell_num_map->rbuf);

	   for(j=0;j < ncol;j++){
	      if(wshd->rbuf[j] > 0){
		cel[ct].cell_num = ct;
		cell_num_map->rbuf[j] = ct;
		ct++;
		fprintf(fd,"%d=%d\n",wshd->rbuf[j],wshd->rbuf[j]);
		}
	   }

	   G_put_map_row(cell_num_map->fd,cell_num_map->rbuf);
	}

	tot_cells = ct - 1;

	fclose(fd);
	G_close_cell(cell_num_map->fd);

	sprintf(buf,"r.reclass input=%s output=MASK < %s",wshd->p,tempfile);
	G_system(buf);

	sprintf(buf,"/bin/rm -f %s",tempfile);
	G_system(buf);
     
        for(i=1; i<=tot_cells; i++) {
          fprintf(temp,"the cell_id and i %i %i \n",cel[i].cell_num,i);
          }
        fclose(temp);










}
