/*
****************************************************************************
*
* MODULE:       g.region.v5d
* AUTHOR(S):    Beverly Wallace - beverly.t.wallace@lmco.com
* PURPOSE:      Set the region corresponding to Vis5d v5d data.
*		Converts the Vis5d region into a Grass raster region.
* COPYRIGHT:    (C) 2001 by Lockheed Martin Space Systems, Sunnyvale, CA, USA 
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

/* Region notes:
 * Vis5d longitudes are reversed (east is negative, west is positive).
 * The Grass raster region is along the edge of the map itself.
 * The Vis5d topo region passes through the center of the grid cells.
 * The number of rows and columns remains the same.
 */


#include <string.h>
#include <stdio.h>
#include <math.h>

#include "config.h"  /* Grass's config.h, not Vis5d's ../config.h */
#include "gis.h"

#include "v5d.h"


static v5dstruct *v5_get_info (char *v5_file, int print_flag)
{
	v5dstruct *v5d_head = NULL;

	/* Open and read the header of the v5d file */
	v5d_head = v5dOpenFile (v5_file, v5d_head);
	if (!v5d_head )
		return NULL;

	/* Close the file */
	v5dCloseFile (v5d_head);

	/* Print the header from the v5d file */
	if (print_flag) v5dPrintStruct (v5d_head);

	return v5d_head;
}


static int v5_region_to_cellhd (v5dstruct *v5_head, struct Cell_head *region) 
{
    /* The resolution, rows, and columns remain the same */
    region->ns_res = v5_head->ProjArgs[2];
    region->ew_res = v5_head->ProjArgs[3];
    region->rows = v5_head->Nr;
    region->cols = v5_head->Nc;

    /* Adjust north and west from center of cell to the edge */
    /* Be sure to reverse the v5d longitude */
    region->north = v5_head->ProjArgs[0] + (.5 * region->ns_res);
    region->west = -(v5_head->ProjArgs[1]) - (.5 * region->ew_res);

    /* Calculate south and east */
    region->south = region->north - (region->rows * region->ns_res);
    region->east = region->west + (region->cols * region->ew_res);

   /* Guess the projection and zone */
    if (region->north > 90) {
	region->proj = 1;	/* Assume UTM */
	region->zone = 0;	/* Don't know which zone */
    }
    else {
	region->proj = 3;	/* Assume lat-lon */
	region->zone = 0;
    }

    return 1;
}


static int set_region (struct Cell_head *region) 
{
    if (G_adjust_Cell_head (region, 0, 0))
	return 0;
    if (G_put_window (region) < 0)
	return 0;

    return 1;
}


int main(int argc, char *argv[])
{
    char v5d_file[128]; 
    int print_header = 0, retain_resol = 0;
    char msg[100];	
    int status;
    v5dstruct *v5_head = NULL;
    struct Cell_head v5d_region, in_region;

    struct {
	struct Option *v5d ;
    } parm;
    struct {
	struct Flag *p;
	struct Flag *r;
    } flags;

    /* Initialize Grass */
    G_gisinit(argv[0]);

    {
    struct GModule *module;
    module = G_define_module();
    module->description =
        "Set the region corresponding to Vis5d v5d data.";
    }

    /* Define the different options and parse them */
    parm.v5d = G_define_option() ;
    parm.v5d->key        = "v5d";
    parm.v5d->type       = TYPE_STRING;
    parm.v5d->required   = YES;
    parm.v5d->description= "Full path to an existing Vis5d v5d file" ;
 
    flags.p = G_define_flag();
    flags.p->key = 'p';
    flags.p->description = "Print information";

    flags.r = G_define_flag();
    flags.r->key = 'r';
    flags.r->description = "Retain the original resolution";

    if (G_parser(argc, argv))
       	return (1);
    strcpy (v5d_file, parm.v5d->answer);
    print_header = flags.p->answer;
    retain_resol = flags.r->answer;

    /* Read the header from the v5d file (optionally print header) */
    if (print_header) {
	fprintf (stdout, "\nVis5d information for %s\n", v5d_file);
	fflush (stdout);
    }
    v5_head = v5_get_info (v5d_file, print_header);

    if (!v5_head) {
	sprintf (msg, "%s: Unable to read v5d file %s\n", 
                G_program_name(), v5d_file);
	G_fatal_error (msg);  /* Exits */
    }

    /* Currently only works for Projection 0 and 1 */
    if (v5_head->Projection > 1) {
	sprintf (msg, "%s: Unable to convert this projection %d\n", 
                G_program_name(), v5_head->Projection);
	G_fatal_error (msg);  /* Exits */
    }

    /* Calculate the Grass region from the v5d region */
    v5_region_to_cellhd (v5_head, &v5d_region);

    /* Get the Grass region */
    G_get_window (&in_region);

    /* Check the projection */
    if (in_region.proj != v5d_region.proj) {
	sprintf (msg, "%s: Projections differ:  GRASS  %d, Vis5d %d\n", 
                G_program_name(), in_region.proj, v5d_region.proj);
	G_fatal_error (msg);  /* Exits */
    }

    /* Use the original resolution with the new NSEW */
    if (retain_resol) {
	v5d_region.ns_res = in_region.ns_res;
	v5d_region.ew_res = in_region.ew_res;
	v5d_region.rows = (v5d_region.north - v5d_region.south) / 
		v5d_region.ns_res;
	v5d_region.cols = (v5d_region.east - v5d_region.west) / 
		v5d_region.ew_res;
    }

    /* Set the new region */
    status = set_region (&v5d_region);
    if (!status) {
	sprintf (msg, "%s: Unable to update current region\n", 
		G_program_name());
    }

    /* Print the Grass region */
    if (print_header) {
	fprintf (stdout, "\nGRASS Region for %s\n", v5d_file);
	fflush (stdout);
	G__write_Cell_head (stdout, &v5d_region, 0);
    }

    return 1;
}

