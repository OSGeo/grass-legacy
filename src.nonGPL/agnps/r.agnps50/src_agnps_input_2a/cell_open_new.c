#include <stdio.h>

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

/*	January, 1991	Agricultural Engineering, Purdue University
	Raghavan Srinivasan (srin@ecn.purdue.edu)
	
	int cell_open_new(name)

	To open a new map with name and in the current mapset and 
	returns the file id.
*/

int cell_open_new(name)
char *name;
{
	int 	fd;

	fd = G_open_cell_new(name);
	if (fd < 0) 
	{
	 	fprintf (stderr,"Oops - can`t open cell file %s\n",name);
	}

	return(fd);

}


