
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
	
	clean_up()

	To remove all the temp maps once the AGNPS input file
	is created. Also, once the user has saved his choice of input
	maps created in this interface tool.

*/

#include "agnps_input.h"

clean_up()
{

	remove_maps("temp_slope");
	remove_maps("temp_drain");
	remove_maps("MASK");
	remove_maps("temp_cn");
	remove_maps("temp_hy_cond");

}

