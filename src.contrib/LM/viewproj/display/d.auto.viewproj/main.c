/*
****************************************************************************
*
* MODULE:       d.auto.viewproj
* AUTHOR(S):    Bev Wallace, beverly.t.wallace@lmco.com
* PURPOSE:      Automatically set the region and projection parameters.
* COPYRIGHT:    (C) 2003 by Lockheed Martin Missiles & Space, Sunnyvale, CA, USA
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

#include <stdlib.h>
#include <unistd.h>	/* For access */
#include <sys/types.h>	/* For mkdir */
#include <sys/stat.h>	/* For mkdir */

#include "config.h"	/* For Grass 5.0 */
#include "gis.h"
#include "projects.h"	/* For PJ & pj */

#include "auto.h"	/* For proj_def_struct & zoom_struct */


int set_region (struct Cell_head *cellhd) 
{
	if (G_adjust_Cell_head (cellhd, 0, 0))
		return 0;
	if (G_put_window (cellhd) < 0)
		return 0;

	fprintf (stdout, "Region was modified for view projection.\n");
	fprintf (stdout, " n=%f s=%f e=%f w=%f\n", 
		cellhd->north, cellhd->south, 
		cellhd->east, cellhd->west); 
	fflush (stdout);
	return 1;
}


int test_proj (char proj_param[MAX_PROJ_PARAMS][MAX_PROJ_PARAM_LEN], 
		int num_param)
{
	/* See if proj will accept the projection */
	PJ *ref;
	int i;
	char *parms[50];

	for (i = 0; i < num_param; i++)
		fprintf (stdout, " %s", proj_param[i]);
	fprintf (stdout, "\n");
	fflush (stdout);

	for (i=0; i<num_param; i++)
		parms[i] = proj_param[i];

	ref = pj_init (num_param, parms);
	if (ref) {
		fprintf (stdout, "Projection was accepted by PROJ\n");
		pj_free(ref);
	}
	else {
		/* Exit due to PROJ error */
 		fprintf(stderr, "pj_init error %d:  %s\n", 
			pj_errno, pj_strerrno(pj_errno));
		G_fatal_error ("PROJ doesn't accept these parameters!");
	}

	return 1;
}


int save_proj (char proj_param[MAX_PROJ_PARAMS][MAX_PROJ_PARAM_LEN], 
		int num_param)
{
	/* Save the projection the general projection information file */
	FILE *file_Ptr;
	int i;

	file_Ptr = G_fopen_new ("viewproj_states" , "current");
	if (file_Ptr == NULL)
                G_fatal_error ("Unable to open file viewproj_states/current");
		
	for (i=0; i<num_param; i++)
		fprintf (file_Ptr, "%s\n", proj_param[i]);

	fclose (file_Ptr);

	return 1;
}


int main (int argc, char **argv)
{
	struct Option *proj_opt;
	struct Flag *region_flag;
	proj_def_struct *map_proj;
	zoom_struct map_region;
	struct Cell_head grass_region;
	int status;
	char param_def[MAX_PROJ_PARAMS][MAX_PROJ_PARAM_LEN];
	char proj_list[MAX_PROJ_LIST];
	int n_def;

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	/* construct proj_list */
	auto_proj_list (proj_list);

	/* Set up command line */
	proj_opt             = G_define_option();
	proj_opt->key         = "projname";
	proj_opt->type        = TYPE_STRING;
	proj_opt->required    = YES;
	proj_opt->options    = proj_list;
	proj_opt->description = "projection to view maps";

	/* Unable to make a flag default to [y] */
	/* Want the default [n] to adjust the region */
	region_flag = G_define_flag();
	region_flag->key = 'r';
	region_flag->description = "retain the current region";

	if (G_parser(argc, argv))
		exit(1);

	/* Verify lat/lon database */
        if (G_projection() != 3)
                G_fatal_error ("Viewproj only works with lat/lon databases\n");

	/* Get the proj_def_struct */
	map_proj = auto_translate_proj (proj_opt->answer);
	if (!map_proj)
		G_fatal_error ("Unable to translate projection");

	/* Get the region */
	G_get_window(&grass_region);
	map_region.north = grass_region.north;
	map_region.south = grass_region.south;
	map_region.east = grass_region.east;
	map_region.west = grass_region.west;


	if (!region_flag->answer) {
		/* Calculate the region for the projection */
		/* Returns 1 if the region was adjusted */
		status = auto_proj_region (map_proj, &map_region);
	}
	else
		status = 0;

	/* Set the Grass region */
	if (status) {
		grass_region.north = map_region.north;
		grass_region.south = map_region.south;
		grass_region.east = map_region.east;
		grass_region.west = map_region.west;
		status = set_region (&grass_region);
		if (!status)
			G_fatal_error ("Unable to update current region");
	}

	/* Calculate the proj parameters for the projection and region */
	n_def = auto_proj_param (map_proj, map_region, param_def);

	/* Set the proj parameters */
	test_proj (param_def, n_def);
	save_proj (param_def, n_def);

	return 1;
}
