
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
	
	int cell_open(name,mapset)

	To open an old map with name and in the mapset and returns the
	file id.
*/

int cell_open(name,mapset)
char *name;
char *mapset;
{
    int fd;
    extern char *sprintf();
    char buf[100];


    if ((fd = G_open_cell_old (name, mapset)) < 0)
    {
	sprintf(buf, "Unable to open file [%s]\n", name);
	G_fatal_error(buf);
	exit(0);
    }

    return(fd);
}
