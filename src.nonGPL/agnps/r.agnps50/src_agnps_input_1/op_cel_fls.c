
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
	
	op_cel_fls()

	To open the cell files for reading so as to
	prepare the agnps input file.
*/

#include "agnps_input.h"


op_cel_fls()
{

/* open the rcell map */
	temp_drain_map->fd = cell_open(temp_drain_map->p, this_mapset);
	temp_drain_map->rbuf = G_allocate_cell_buf();

/* open the slope map */
	temp_slope_map->fd = cell_open(temp_slope_map->p, this_mapset);
	temp_slope_map->rbuf = G_allocate_cell_buf();

/* open the aspect map */
	temp_dir_map->mapset = get_mapset(temp_dir_map->p);
	temp_dir_map->fd = cell_open(temp_dir_map->p, temp_dir_map->mapset);
	temp_dir_map->rbuf = G_allocate_cell_buf();

/* open the hy_cond map */
	hy_cond->fd = cell_open(hy_cond->p, this_mapset);       
	hy_cond->rbuf = G_allocate_cell_buf();

/* open the CN map */
	temp_cn_map->fd = cell_open(temp_cn_map->p, this_mapset);       
	temp_cn_map->rbuf = G_allocate_cell_buf();

/* open the C map */
	C_fac->mapset = get_mapset(C_fac->p);
	C_fac->fd = cell_open(C_fac->p, C_fac->mapset);       
	C_fac->rbuf = G_allocate_cell_buf();

/* open the machinery  map */
	machinery->mapset = get_mapset(machinery->p);
	machinery->fd = cell_open(machinery->p, machinery->mapset);
	machinery->rbuf = G_allocate_cell_buf();
}
