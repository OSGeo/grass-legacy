/*
****************************************************************************
*
* MODULE:       r.out.v5d.topo
* AUTHOR(S):    Beverly Wallace - beverly.t.wallace@lmco.com
* PURPOSE:      To convert Grass raster data into Vis5d topo data.
*               Based upon the GRASS 5 r.out.ascii code (May 2001).
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
#include <float.h>

#include "config.h"  /* Grass's config.h, not Vis5d's ../config.h */
#include "gis.h"


extern int topo_init (int rows, int cols,
			float north, float south, float east, float west,
			char *filename);
extern int topo_data (int fd, float *Topo, int array_size);
extern int topo_final (int fd);

/* Uncomment for debug messages - Bev Wallace */
/* #define DEBUG */


int convert_raster (char *name, char *mapset, char *outfile) 
{
    /* Purpose:  Convert Grass raster data to Vis5d topo data */

    int fd, topo_fd;
    int row,col;
    int nrows, ncols;
    CELL idata;
    FCELL fdata;
    DCELL ddata;
    struct Cell_head region;
    int index = 0;
    RASTER_MAP_TYPE map_type;
    void *raster, *ptr;
    float *Topo;
    int status = 1;
   char msg[100];	
   
    /* Open the Grass file */
    fd = G_open_cell_old (name, mapset);
    if (fd < 0) {
        sprintf (msg, "%s: Unable to open raster file %s@%s\n", 
		G_program_name(), name, mapset);
	G_fatal_error (msg);  /* Exits */
    }

    /* Get the Grass region */
    map_type = G_raster_map_type (name, mapset);
    G_get_window (&region);
    nrows = G_window_rows();
    ncols = G_window_cols();

#ifdef DEBUG
    fprintf (stdout, "Grass window:\n");
    fprintf (stdout, " nrows=%d, ncols=%d\n", nrows, ncols);
    fprintf (stdout, " north=%f, south=%f\n", region.north, region.south);
    fprintf (stdout, " east=%f, west=%f\n", region.east, region.west);
    if (map_type == CELL_TYPE)
    	fprintf (stdout, " map_type == CELL_TYPE\n");
    else if (map_type == FCELL_TYPE)
    	fprintf (stdout, " map_type == FCELL_TYPE\n");
    else if (map_type == DCELL_TYPE)
    	fprintf (stdout, " map_type == DCELL_TYPE\n");
#endif /* DEBUG */

    /* Open the Vis5d file and write the region */
    /* Vis5d uses the center of the Grass cells for NSEW */
    /* Vis5d longitudes are reversed:  West is positive and East is negative */
    topo_fd = topo_init (nrows, ncols,
        region.north - (.5 * region.ns_res), 
	region.south + (.5 * region.ns_res), 
	-(region.east - (.5 * region.ew_res)), 
	-(region.west + (.5 * region.ew_res)),
        outfile);

    if (topo_fd < 0) {
        sprintf (msg, "%s: Unable to open Topo file %s for writing\n", 
		G_program_name(), outfile);
	G_fatal_error (msg);  /* Exits */
    }

    /* Allocate for one row of data at a time */
    Topo = (float *) G_malloc (ncols * sizeof (float));
    if (!Topo) {
        sprintf (msg, "%s: Error allocating Topo data, size=%d\n", 
		G_program_name(), nrows*ncols);
	G_fatal_error (msg);  /* Exits */
    }

    /* Read rows of Grass data and write Vis5d data */
    raster =  G_allocate_raster_buf (map_type);
    for (row = 0; row < nrows; row++) {
	if (G_get_raster_row (fd, raster, row, map_type) < 0) {
        	sprintf (msg, "%s: Error in G_get_raster_row row=%d\n", 
			G_program_name(), row);
		G_fatal_error (msg);  /* Exits */
	}
	ptr = raster;
	index = 0;
        for (col = 0; col < ncols; col++) {
		if (G_is_null_value (ptr, map_type)) {
			Topo [index] = (float) FLT_MAX;
		}
		else if (map_type == CELL_TYPE) {
			idata = *((CELL *) ptr);
			Topo [index] = (float) idata;
		}
		else if (map_type == FCELL_TYPE) {
			fdata = *((FCELL *) ptr);
			Topo [index] = (float) fdata;
		}
		else if (map_type == DCELL_TYPE) {
			ddata = *((DCELL *) ptr);
			Topo [index] = (float) ddata;
		}
		ptr = G_incr_void_ptr (ptr, G_raster_size(map_type));
		index++;
        }
    	status = topo_data (topo_fd, Topo, ncols);
	if (!status) {
		G_close_cell (fd);
        	sprintf (msg, "%s: Error writing Topo data\n", 
			G_program_name());
		G_fatal_error (msg);  /* Exits */
	}
	/* Print out our progress */
	G_percent (row+1, nrows, 2);
    }

    /* Close the Grass and Vis5d files */
    topo_final (topo_fd);
    G_close_cell(fd);

    return 1;
}



int main(int argc, char *argv[])
{
    char *name; 
    char *mapset;
    char outfile[128]; 

    struct
    {
	struct Option *input ;
	struct Option *output ;
    } parm;

    /* Initialize Grass */
    G_gisinit(argv[0]);

    {
    struct GModule *module;
    module = G_define_module();
    module->description =
		"Converts a raster map layer into a Vis5d topo file.";
    }

    /* Define the different options and parse them */
    parm.input = G_define_option() ;
    parm.input->key        = "input";
    parm.input->type       = TYPE_STRING;
    parm.input->required   = YES;
    parm.input->gisprompt  = "old,cell,raster" ;
    parm.input->description= "Name of an existing GRASS raster map" ;

    parm.output = G_define_option() ;
    parm.output->key        = "output";
    parm.output->type       = TYPE_STRING;
    parm.output->required   = YES;
    parm.output->description= "Name of an output Vis5d topo file";

    if (G_parser(argc, argv))
       	return (1);
    strcpy (outfile, parm.output->answer);

    /* Find the Grass file */
    name = parm.input->answer;
    mapset = G_find_cell2 (name, "");
    if (mapset == NULL)
    {
        char msg[100];
	sprintf (msg, "%s: <%s> cellfile not found\n", G_program_name(), name);
	G_fatal_error (msg);  /* Exits */
    }

    /* Convert the raster data to Vis5d topo data */
    convert_raster (name, mapset, outfile);

    return 1;
}

