
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
	
	int find_old_map(map_name)

	map_name: the map name to find in the database

	To find a old map name in any mapset.
	It checks for the existance fo the map layer
	and returns name, if succeds else returns -1.
*/

#include <stdio.h>

int find_old_map(map_name)
char *map_name;
{
	char *mapset, buf[512];
	extern char *G_find_file();

	mapset = G_find_file("cell",map_name,"");
	if(mapset == NULL){
	     sprintf(buf, "Cell layer [%s] not found \n",map_name);
	     G_warning(buf);
	     return(-1);
	     }
	else return(1);
}
