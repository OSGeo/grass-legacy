/*
****************************************************************************
*
* MODULE:       v.out.v5d.map
* AUTHOR(S):    Beverly Wallace - beverly.t.wallace@lmco.com
* PURPOSE:      To convert Grass vector data into Vis5d map outline data.
*               Based upon the GRASS 5 v.out.dlg code (May 2001).
* COPYRIGHT:    (C) 2001 by Lockheed Martin Space Systems, Sunnyvale, CA, USA 
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

/* Region notes:
 * Vis5d longitudes are reversed (east is negative, west is positive) 
 */


#include <string.h>
#include <stdio.h>
#include <math.h>

#include "config.h"  /* Grass's config.h, not Vis5d's ../config.h */
#include "gis.h"
#include "Vect.h"


/* Declare Vis5d map outline functions from makemap.c */
extern int vertex (float lat, float lon);
extern int end_line ();
extern int done (char *filename);


int convert_vector (char *name, char *mapset, char *outfile) 
{
    /* Purpose:  Convert Grass vector data to Vis5d map data */

    int level;
    struct Cell_head window;
    struct Map_info Map;
    int i;
    struct line_pnts *Points;
    int status;
    char msg[100];	

    /* Open the Grass vector file */
    level = Vect_open_old (&Map, name, mapset);
    if (level <= 0) {
	sprintf (msg, "%s: Unable to open vector file %s@%s\n", 
		G_program_name(), name, mapset);
	G_fatal_error (msg);  /* Exits */
    }
    if (level < 2 || !Map.all_areas || !Map.all_isles) {
	sprintf (msg, "%s: support.vect must be run for %s@%s\n", 
		G_program_name(), name, mapset);
	G_fatal_error (msg);  /* Exits */
    }

    /* Constrict region */
    G_get_set_window (&window);
    Vect_set_constraint_region (&Map, window.north, window.south, 
	window.east, window.west);

    /* Read Grass vector data, store for Vis5d */
    /* Vis5d longitudes are reversed:  West is positive and East is negative */
    Points = Vect_new_line_struct ();
    while (Vect_read_next_line (&Map, Points) > 0) {
	for (i=0; i < Points->n_points; i++)
		vertex (Points->y[i], -(Points->x[i]));
	end_line();
    }
    Vect_destroy_line_struct (Points);

    /* Write the Vis5d file */
    status = done (outfile);
    if (!status)  {
	sprintf (msg, "%s: Unable to write map file %s\n", 
		G_program_name(), outfile);
	G_fatal_error (msg);  /* Exits */
    }

    /* Close the Vis5d file */
    Vect_close (&Map);

    return 1;
}



int main (int argc, char *argv[])
{
    char *name; 
    char *mapset;
    char outfile[128]; 
    char msg[100];	
    struct GModule *module;

    struct
    {
	struct Option *input ;
	struct Option *output ;
    } parm;

    /* Initialize Grass */
    G_gisinit(argv[0]);

    module = G_define_module();
    module->description =
	"Converts a Grass vector map into a Vis5d map outline file.";

    /* Define the different options and parse them */
    parm.input = G_define_option() ;
    parm.input->key        = "input";
    parm.input->type       = TYPE_STRING;
    parm.input->required   = YES;
    parm.input->gisprompt  = "old,dig,vector" ;
    parm.input->description= "Name of an existing GRASS vector map" ;

    parm.output = G_define_option() ;
    parm.output->key        = "output";
    parm.output->type       = TYPE_STRING;
    parm.output->required   = YES;
    parm.output->description= "Name of an output Vis5d map outline file";

    if (G_parser(argc, argv))
       	return (1);
    strcpy (outfile, parm.output->answer);

    /* Find the Grass file */
    name = parm.input->answer;
    mapset = G_find_vector2 (name, "");
    if (mapset == NULL) {
	sprintf (msg, "%s: <%s> Vector file not found\n", G_program_name(), name);
	G_fatal_error (msg);  /* Exits */
    }

    /* Convert the vector data to Vis5d map data */
    convert_vector (name, mapset, outfile);

    return (0);
}

