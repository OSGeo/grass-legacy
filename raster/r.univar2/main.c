/* $Id$ 
 * r.univar
 *
 *  Calculates univariate statistics from the non-null cells of a GRASS raster map
 *
 *   Copyright (C) 2004 by the GRASS Development Team
 *   Author: Hamish Bowman, University of Otago, New Zealand
 *
 *   This program is free software under the GPL (>=v2)
 *   Read the COPYING file that comes with GRASS for details.
 *
 *   This program is a replacement for the r.univar shell script
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "gis.h"


int main(int argc, char *argv[]) {

    unsigned int row,col;        /* counters */
    unsigned int rows,cols;      /*  totals  */

    int val_i;		/* for misc use */
    float val_f;	/* for misc use */
    double val_d;	/* for misc use */
    int first = TRUE;   /* min/max init flag */

    unsigned int n = 0;
    double sum = 0.;
    double sumsq = 0.;
    double min = 0.0/0.0; /* init as nan */
    double max = 0.0/0.0; /* init as nan */
    double mean, stdev, var_coef;
    double variance;

    char *infile, *mapset;
    void *raster_row, *ptr;
    RASTER_MAP_TYPE map_type;
    struct Cell_head region;
    int fd;

    struct Option *inputfile;
    struct Flag *quiet, *shell_style;   /* , *extended; */
    struct GModule *module;


    G_gisinit(argv[0]);

    module = G_define_module();
    module->description =
	"Calculates univariate statistics from the non-null cells of a raster map.";

    /* Define the different options */

    inputfile = G_define_option() ;
    inputfile->key        = "input";
    inputfile->type       = TYPE_STRING;
    inputfile->required   = YES;
    inputfile->gisprompt  = "old,cell,raster" ;
    inputfile->description= "Name of an existing raster map";

    quiet = G_define_flag();
    quiet->key = 'q';
    quiet->description = "Quiet mode";

    shell_style = G_define_flag();
    shell_style->key = 'g';
    shell_style->description = "Print the stats in shell script style";

    /*** Not yet implemented: Median, 1st Quartile, 3rd Quartile ***
    extended = G_define_flag();
    extended->key = 'e';
    extended->description = "Calculate extended statistics";
    ***/

    if (G_parser(argc,argv))
	exit(1);

    infile = inputfile->answer;

    mapset = G_find_cell2 (infile, "");
    if (mapset == NULL) {
	G_fatal_error("raster <%s> not found\n", infile);
    }

    fd = G_open_cell_old (infile, mapset);
    if (fd < 0)
	G_fatal_error("unable to open <%s>\n", infile);

    map_type = G_raster_map_type(infile, mapset);

    G_get_window (&region);
    rows = region.rows;
    cols = region.cols;

    raster_row = G_calloc(G_window_cols()+1, G_raster_size(map_type));

    if( ! (quiet->answer || shell_style->answer) ) {
	fprintf(stderr, "\nProcessing .. ");
	fflush(stderr);
    }

    for(row = 0; row<rows; row++) {
	if (G_get_raster_row(fd, raster_row, row, map_type) < 0)
	    G_fatal_error("reading map");

	ptr = raster_row;

	for(col=0; col<cols; col++) {

	    if(G_is_null_value(ptr, map_type)) {
		ptr = G_incr_void_ptr(ptr, G_raster_size(map_type));
		continue;
	    }

	    n++;

	    if(map_type == CELL_TYPE) {
		val_i = *((CELL *) ptr);

		sum += val_i;
		sumsq += (val_i*val_i);

		if(first) {
		    max = val_i;
		    min = val_i;
		    first = FALSE;
		}
		else {
		    if(val_i > max)
			max = val_i;
		    if(val_i < min)
			min = val_i;
		}
	    }
	    else if(map_type == FCELL_TYPE) {
		val_f = *((FCELL *) ptr);

		sum += val_f;
		sumsq += (val_f*val_f);

		if(first) {
		    max = val_f;
		    min = val_f;
		    first = FALSE;
		}
		else {
		    if(val_f > max)
			max = val_f;
		    if(val_f < min)
			min = val_f;
		}
	    }
	    else if(map_type == DCELL_TYPE) {
		val_d = *((DCELL *) ptr);

		sum += val_d;
		sumsq += val_d*val_d;

		if(first) {
		    max = val_d;
		    min = val_d;
		    first = FALSE;
		}
		else {
		    if(val_d > max)
			max = val_d;
		    if(val_d < min)
			min = val_d;
		}
	    }
	    ptr = G_incr_void_ptr(ptr, G_raster_size(map_type));
	}
	if( ! (quiet->answer || shell_style->answer) )
	    G_percent(row, rows, 2);
    }
    if( ! (quiet->answer || shell_style->answer) )
	G_percent(row, rows, 2);  /* finish it off */

    /* all these calculations get promoted to doubles, so any DIV0 becomes nan */
    mean = sum/n;
    variance = (sumsq - sum*sum/n)/n;
    stdev = sqrt(variance);
    var_coef = (stdev/mean)*100;  /* perhaps stdev/fabs(mean) ? */


    if( ! (quiet->answer || shell_style->answer) ) {
	fprintf(stdout, "\ntotal null and non-null cells: %d\n", rows * cols);
	fprintf(stdout, "total null cells: %d\n\n", rows * cols - n);
	fprintf(stdout, "Of the non-null cells:\n----------------------\n");
    }

/* TODO: median, mode */
    if(shell_style->answer) {
    	fprintf(stdout, "n=%d\n", n);
	fprintf(stdout, "min=%g\n", min);
	fprintf(stdout, "max=%g\n", max);
	fprintf(stdout, "range=%g\n", max - min);
	fprintf(stdout, "mean=%g\n", mean);
	fprintf(stdout, "stddev=%g\n", stdev);
	fprintf(stdout, "variance=%g\n", variance);
	fprintf(stdout, "coeff_var=%g\n", var_coef);
	fprintf(stdout, "sum=%g\n", sum);
    }
    else {
	fprintf(stdout, "n: %d\n", n);
	fprintf(stdout, "minimum: %g\n", min);
	fprintf(stdout, "maximum: %g\n", max);
	fprintf(stdout, "range: %g\n", max - min);
	fprintf(stdout, "mean: %g\n", mean);
	fprintf(stdout, "standard deviation: %g\n", stdev);
	fprintf(stdout, "variance: %g\n", variance);
	fprintf(stdout, "variation coefficient: %g %%\n", var_coef);
	fprintf(stdout, "sum: %g\n", sum);
    }

/*  if(extended->answer) {
	bin_sort();
	q1=;
	q3=;
	median=;
    }
*/

    if( ! ( quiet->answer || shell_style->answer) )
	fprintf(stdout, "\n");

    return 0;
}
