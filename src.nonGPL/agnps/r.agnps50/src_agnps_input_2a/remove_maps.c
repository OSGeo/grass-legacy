
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

/*      June, 1991  Agricultural Engineering, Purdue University
	Raghavan Srinivasan (srin@ecn.purdue.edu)

	remove_maps(name)
	
	name: is the layer name to be removed

*/

#include <stdio.h>

remove_maps(old_name)
char	*old_name;
{
	G_remove("cell",old_name);
	G_remove("colr",old_name);
	G_remove("cats",old_name);
	G_remove("cellhd",old_name);
	G_remove("hist",old_name);
	G_remove("cell_misc",old_name);
}
