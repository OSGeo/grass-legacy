/*
****************************************************************************
*
* MODULE:       d.mon.viewproj
* AUTHOR(S):    Sharif Razzaque, LMMS, June 1995
*               Bev Wallace, beverly.t.wallace@lmco.com
* PURPOSE:      To update the projection associated with the current monitor.
* COPYRIGHT:    (C) 1995 by Lockheed Martin Missiles & Space, Sunnyvale, CA, USA
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

/*
*************************************************************************
d.mon.viewproj

Use this command to update the projection associated with the current monitor.
Just like one uses d.erase to update the region associated with each monitor.

This copies the current projection information (set by d.set.viewproj)
into the projection infomation file for the currently selected monitor.
All these info files are under viewproj_states in the current mapset.

note: there is no interactive version & this executable takes no parameters

Sharif Razzaque June 1995
*************************************************************************
*/

#include "config.h"	/* For Grass 5.0 Bev Wallace */
#include "gis.h"
#include "projects.h"


int main (int argc, char **argv)
{
	char *monitor_name;
	FILE *curr_file_Ptr;
	char curr_file_path[200];
        char mon_file_path[200];
	char command[400];


/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

/* first get name of current monitor */
	monitor_name=G_getenv("MONITOR");
	if (monitor_name==NULL)
		exit(1);

/* init the path names for the relevent files */
	sprintf(curr_file_path,
		"%s/%s/viewproj_states/current",
		G_location_path(), G_mapset());
        sprintf(mon_file_path,
		"%s/%s/viewproj_states/%s",
		G_location_path(), G_mapset(), monitor_name);

/* check if the "current" projection info file is available */
 		
	curr_file_Ptr=fopen(curr_file_path,"r"); 

        if (curr_file_Ptr==NULL)
	{
		fprintf (stderr, "Cannot open %s.\n", curr_file_path);
		fprintf (stderr, "Try running d.set.viewproj first.\n");
		exit(1);
	}
	else 
		fclose(curr_file_Ptr);

/* copy the current projection file to the one specific to the current monitor */
        sprintf(command,"cp %s %s",curr_file_path,mon_file_path);
        system(command);

	exit(0);
}

