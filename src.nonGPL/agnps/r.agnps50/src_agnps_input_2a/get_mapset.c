
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
	
	char *get_mapset(name)

	name:   Name of the map whose mapset is needed.

	To return the mapset of the map layer with the name. 
	The mapset location is returned. 
*/

#include <stdio.h>


char *get_mapset(name)
char *name;
{
	char *mapset, buf[512];
	extern char *G_find_cell();

	mapset = G_find_cell(name,"");
	if(mapset == NULL){
	     sprintf(buf, "Cell layer [%s] not found in any of the mapsets\n",name);
	     G_fatal_error(buf);
	     clean_up();
	     exit(1);
	     }
	else return(mapset);
}
