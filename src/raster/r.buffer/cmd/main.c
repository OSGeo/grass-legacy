/* This program creates distance zones from non-zero
 * cells in a grid layer. Distances are specified in
 * meters (on the command-line). Window does not have to
 * have square cells. Works both for planimetric (UTM, State Plane)
 * and lat-long.
 *
 * Author: Michael Shapiro. US Army CERL.
 */

#define GLOBAL
#include "distance.h"
#include "local_proto.h"
#include <stdio.h>
#include <string.h>

int main (int argc, char *argv[])
{
    struct Distance *pd;
    char *input, *output, *mapset;
    char *type;
    char **zone_list;
    char buf[512];
    double dd;
    double to_meters;
    char *units;
    int offset;
    int count, i, k;
    int step, nsteps;
    struct History hist;
    int quiet;
	struct GModule *module;
    struct Option *opt1, *opt2, *opt3, *opt4;
    struct Flag *flag1;
    struct Flag *flag2;
    int ZEROFLAG;

	/* initialize GRASS */


    init_grass(pgm_name = argv[0]);

	module = G_define_module();
	module->description =
		"Creates a raster map layer showing buffer zones "
		"surrounding cells that contain non-NULL category values.";

    opt1 = G_define_option() ;
    opt1->key        = "input" ;
    opt1->type       = TYPE_DOUBLE;
    opt1->required   = YES ;
    opt1->gisprompt  = "old,cell,raster" ;
    opt1->description= "Name of input map" ;

    opt2 = G_define_option() ;
    opt2->key        = "output" ;
    opt2->type       = TYPE_STRING ;
    opt2->required   = YES ;
    opt2->gisprompt  = "new,cell,raster" ;
    opt2->description= "Name of output map" ;

    opt3 = G_define_option() ;
    opt3->key        = "distances" ;
    opt3->type       = TYPE_DOUBLE;
    opt3->required   = YES ;
    opt3->multiple   = YES;
    opt3->description= "Distance zone(s)" ;

    opt4 = G_define_option() ;
    opt4->key        = "units" ;
    opt4->options    = "meters,kilometers,feet,miles";
    opt4->type       = TYPE_STRING ;
    opt4->required   = NO ;
    opt4->description= "Units of distance" ;
    opt4->answer     = "meters";

    flag1 = G_define_flag() ;
    flag1->key         = 'q';
    flag1->description = "Run quietly";

    flag2 = G_define_flag() ;
    flag2->key         = 'z' ;  
    flag2->description = "Ignore zero (0) data cells instead of NULL cells" ;

    if (G_parser(argc, argv))
        exit(-1);

	/* get input, output map names */


    input     = opt1->answer; 
    output    = opt2->answer; 
    zone_list = opt3->answers;
    units     = opt4->answer;

    quiet = flag1->answer;
    ZEROFLAG = 0; /* default: use NULL for non-data cells */
    ZEROFLAG = (flag2->answer);                                               
        
    mapset = G_find_cell (input, "");
    if (mapset == NULL)
    {
	fprintf (stderr, "%s: %s - not found\n", pgm_name, input);
	exit(1);
    }

    if (G_legal_filename(output) < 0)
    {
	fprintf (stderr, "%s: %s - illegal name\n", pgm_name, output);
	exit(1);
    }

/* Initialze History */
	type = "raster";
	G_short_history(output, type, &hist);

        /* parse units */
    if (opt4->answer == NULL)
	units = "meters";

    if (strcmp(units, "meters") == 0)
	to_meters = 1.0;
    else if (strcmp(units, "feet") == 0)
	to_meters = FEET_TO_METERS;
    else if (strcmp(units, "kilometers") == 0)
	to_meters = KILOMETERS_TO_METERS;
    else if (strcmp(units, "miles") == 0)
	to_meters = MILES_TO_METERS;
    else
    {
	fprintf (stderr, "%s: %s - illegal units\n", pgm_name, units);
	G_usage();
	exit(1);
    }


	/* parse distances */
    if(!(count = parse_distances (zone_list, to_meters)))
    {
	G_usage();
	exit(1);
    }



	/* need to keep track of distance zones - in memory.
	 * process MAX_DIST at a time
	 *
	 * Coding: 0 == not-yet determined, 1 == input cells,
	 *         2 == distance zone #1,   3 == distance zone #2, etc.
	 */

    read_input_map (input, mapset, quiet, ZEROFLAG);

    offset = 0;

    nsteps = (count - 1) / MAX_DIST + 1;

/* Write out History Structure History */
	sprintf(hist.title, "%s", output);
	sprintf(hist.datsrc_1, "%s", input);
	sprintf(hist.edhist[0], "Created from: r.buffer");
	sprintf(hist.edhist[1], "input=%s output=%s units=%s", input, output, units);
	sprintf(hist.edhist[2], "distances=");

	for (i=0; opt3->answers[i]; i++) {
	k = i;
	}
	for (i=0; i <= k ; i++) {
	sscanf(opt3->answers[i], "%lf", &dd);
	sprintf(buf, "%.2f ", dd);
	strcat(hist.edhist[2], buf);
	}
	hist.edlinecnt = k+1;
/* Done with history */

	
    pd = distances;
    for (step = 1; count > 0; step++)
    {
       if ( ! quiet)
	  if (nsteps > 1) fprintf (stderr, "Pass %d (of %d)\n", step, nsteps);
        ndist = count;
	if (ndist > MAX_DIST)
	    ndist = MAX_DIST;
	if(count_rows_with_data > 0) 
	      execute_distance(quiet);
	write_output_map(output, offset, quiet);
	offset += ndist;
	distances += ndist;
	count -= ndist;
    }
    distances = pd;
    make_support_files (output, units);

/* Write out New History */
	G_write_history (output, &hist);

    exit(0);
}
