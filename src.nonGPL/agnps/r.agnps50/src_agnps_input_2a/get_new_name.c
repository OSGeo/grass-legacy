
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
	
	char *get_new_name(promt,name)

	promt:  any string to give info to the user what the program expects.
	name :  name of the new map layer.

	To get the new map name and in the current mapset using 
	the specified prompt. It checks for the existance fo the map layer
	with the same name and returns name, if succeds else quits.
*/

#include <stdio.h>

char *get_new_name(prompt)
char *prompt;
{
	char name[50], *mapset, buf[512];
	extern char *G_ask_cell_new();

	mapset = G_ask_cell_new(prompt,name);
	if(mapset == NULL){
	     sprintf(buf, "Cell layer [%s] not acceptable \n",name);
	     G_fatal_error(buf);
	     clean_up();
	     exit(1);
	     }
	else return(name);
}
