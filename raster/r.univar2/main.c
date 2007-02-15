/*
 * r.univar
 *
 *  Calculates univariate statistics from the non-null cells of a GRASS raster map
 *
 *   Copyright (C) 2004-2006 by the GRASS Development Team
 *   Author(s): Hamish Bowman, University of Otago, New Zealand
 *              Extended stats: Martin Landa
 *
 *      This program is free software under the GNU General Public
 *      License (>=v2). Read the file COPYING that comes with GRASS
 *      for details.
 *
 *   This program is a replacement for the r.univar shell script
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <grass/gis.h>
#include <grass/glocale.h>

/* fn prototypes */
static int cmp_int (const void *, const void *);
static int cmp_float (const void *, const void *);
static int cmp_double (const void *, const void *);


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
    double sum_abs = 0.;
    int perc;
    void *pvals = NULL; /* array to hold extended stats */

    char sum_str[100];

    char *infile, *mapset;
    void *raster_row, *ptr;
    RASTER_MAP_TYPE map_type;
    struct Cell_head region;
    int fd;

    struct Option *inputfile, *percentile;
    struct Flag *shell_style, *extended;
    struct GModule *module;


    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("raster, statistics");
    module->description =
	_("Calculates univariate statistics from the non-null cells of a raster map.");

    /* Define the different options */

    inputfile = G_define_standard_option(G_OPT_R_MAP);

    percentile = G_define_option();
    percentile->key      = "percentile";
    percentile->type     = TYPE_INTEGER;
    percentile->required = NO;
    percentile->options  = "0-100";
    percentile->answer   = "90";
    percentile->description =
	_("Percentile to calculate (requires extended statistics flag)");


    shell_style = G_define_flag();
    shell_style->key = 'g';
    shell_style->description = _("Print the stats in shell script style");

    extended = G_define_flag();
    extended->key = 'e';
    extended->description = _("Calculate extended statistics");

    if (G_parser(argc,argv))
	exit(EXIT_FAILURE);


    infile = inputfile->answer;

    perc = atoi(percentile->answer);

    mapset = G_find_cell2 (infile, "");
    if (mapset == NULL) {
	G_fatal_error(_("Raster map [%s] not found"), infile);
    }

    fd = G_open_cell_old (infile, mapset);
    if (fd < 0)
	G_fatal_error(_("Unable to open raster map [%s]"), infile);

    map_type = G_get_raster_map_type(fd);

    if(extended->answer) {
	pvals = G_calloc(G_window_rows() * G_window_cols() + 1,
			 G_raster_size(map_type));
    }

    G_get_window (&region);
    rows = region.rows;   /* use G_window_rows(), G_window_cols() here? */
    cols = region.cols;

    raster_row = G_calloc(G_window_cols()+1, G_raster_size(map_type));

    for(row = 0; row<rows; row++) {
	if (G_get_raster_row(fd, raster_row, row, map_type) < 0)
	    G_fatal_error(_("Reading row %d"), row);

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
		sum_abs += abs(val_i);

		if(extended->answer)
		    ((CELL *) pvals)[n-1] = val_i;

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
		sum_abs += fabs(val_f);

		if (extended->answer)
		    ((FCELL *) pvals)[n-1] = val_f;

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
		sum_abs += fabs(val_d);

		if(extended->answer)
		    ((DCELL *) pvals)[n-1] = val_d;

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
	if( ! (shell_style->answer) )
	    G_percent(row, rows, 2);
    }
    if( ! (shell_style->answer) )
	G_percent(row, rows, 2);  /* finish it off */

    /* all these calculations get promoted to doubles, so any DIV0 becomes nan */
    mean = sum/n;
    variance = (sumsq - sum*sum/n)/n;
    if (variance < GRASS_EPSILON )
        variance=0.0;
    stdev = sqrt(variance);
    var_coef = (stdev/mean)*100.;  /* perhaps stdev/fabs(mean) ? */

    sprintf(sum_str, "%.10f", sum);
    G_trim_decimal(sum_str);


    if( !shell_style->answer ) {
	fprintf(stdout, "total null and non-null cells: %d\n", rows * cols);
	fprintf(stdout, "total null cells: %d\n\n", rows * cols - n);
	fprintf(stdout, "Of the non-null cells:\n----------------------\n");
    }


    if(shell_style->answer) {
    	fprintf(stdout, "n=%d\n", n);
	fprintf(stdout, "min=%g\n", min);
	fprintf(stdout, "max=%g\n", max);
	fprintf(stdout, "range=%g\n", max - min);
	fprintf(stdout, "mean=%g\n", mean);
	fprintf(stdout, "mean_of_abs=%g\n", sum_abs / n);
	fprintf(stdout, "stddev=%g\n", stdev);
	fprintf(stdout, "variance=%g\n", variance);
	fprintf(stdout, "coeff_var=%g\n", var_coef);
	fprintf(stdout, "sum=%s\n", sum_str);
    }
    else {
	fprintf(stdout, "n: %d\n", n);
	fprintf(stdout, "minimum: %g\n", min);
	fprintf(stdout, "maximum: %g\n", max);
	fprintf(stdout, "range: %g\n", max - min);
	fprintf(stdout, "mean: %g\n", mean);
	fprintf(stdout, "mean of absolute values: %g\n", sum_abs / n);
	fprintf(stdout, "standard deviation: %g\n", stdev);
	fprintf(stdout, "variance: %g\n", variance);
	fprintf(stdout, "variation coefficient: %g %%\n", var_coef);
	fprintf(stdout, "sum: %s\n", sum_str);
    }


/* TODO: mode, skewness, kurtosis */
    if(extended->answer) {
	double quartile_25 = 0.0, quartile_75 = 0.0, quartile_perc = 0.0;
	double median = 0.0;
	int qpos_25, qpos_75, qpos_perc;

	qpos_25   = (int) (n * 0.25 - 0.5);
	qpos_75   = (int) (n * 0.75 - 0.5);
	qpos_perc = (int) (n * perc/100. - 0.5);

	switch (map_type)
	{
	case CELL_TYPE:
	    qsort (pvals, n, G_raster_size(map_type), cmp_int);

	    quartile_25 = ((CELL*) pvals)[qpos_25];
	    if (n % 2) /* odd */
		median = ((CELL*) pvals)[(int) (n/2)];
	    else /* even */
		median = ( ((CELL*) pvals)[n/2 -1] + ((CELL*) pvals)[n/2] ) / 2.0;
	    quartile_75 = ((CELL*) pvals)[qpos_75];
	    quartile_perc = ((CELL*) pvals)[qpos_perc];
	    break;

	case FCELL_TYPE:
	    qsort (pvals, n, G_raster_size(map_type), cmp_float);

	    quartile_25 = ((FCELL*) pvals)[qpos_25];
	    if (n % 2) /* odd */
		median = ((FCELL*) pvals)[(int) (n/2)];
	    else /* even */
		median = ( ((FCELL*) pvals)[n/2 -1] + ((FCELL*) pvals)[n/2] ) / 2.0;
	    quartile_75 = ((FCELL*) pvals)[qpos_75];
	    quartile_perc = ((FCELL*) pvals)[qpos_perc];
	    break;

	case DCELL_TYPE:
	    qsort (pvals, n, G_raster_size(map_type), cmp_double);
	    
	    quartile_25 = ((DCELL*) pvals)[qpos_25];
	    if (n % 2) /* odd */
		median = ((DCELL*) pvals)[(int) (n/2)];
	    else /* even */
		median = ( ((DCELL*) pvals)[n/2 -1] + ((DCELL*) pvals)[n/2] ) / 2.0;
	    quartile_75 = ((DCELL*) pvals)[qpos_75];
	    quartile_perc = ((DCELL*) pvals)[qpos_perc];
	    break;

	default:
	    break;
	}

	if(shell_style->answer) {
	    fprintf(stdout, "first_quartile=%g\n", quartile_25);
	    fprintf(stdout, "median=%g\n", median);
	    fprintf(stdout, "third_quartile=%g\n", quartile_75);
	    fprintf(stdout, "percentile_%d=%g\n", perc, quartile_perc);
	}
	else {
	    fprintf(stdout, "1st quartile: %g\n", quartile_25);
	    if (n % 2)
	        fprintf(stdout, "median (odd number of cells): %g\n", median);
	    else
	        fprintf(stdout, "median (even number of cells): %g\n", median); 
	    fprintf(stdout, "3rd quartile: %g\n", quartile_75);

	    if(perc%10 == 1 && perc != 11)
		fprintf(stdout, "%dst percentile: %g\n", perc, quartile_perc);
	    else if(perc%10 == 2 && perc != 12 )
		fprintf(stdout, "%dnd percentile: %g\n", perc, quartile_perc);
	    else if(perc%10 == 3 && perc != 13 )
		fprintf(stdout, "%drd percentile: %g\n", perc, quartile_perc);
	    else
		fprintf(stdout, "%dth percentile: %g\n", perc, quartile_perc);
	}
    }


    if( ! ( shell_style->answer) )
	G_message("\n");


    exit(EXIT_SUCCESS);
}


static int cmp_int (const void *pa, const void *pb)
{
    int *p1 = (int *) pa;
    int *p2 = (int *) pb;

    if (*p1 < *p2)
	return -1;
    if (*p1 > *p2)
	return 1;

    return 0;
}

static int cmp_float (const void *pa, const void *pb)
{
    float *p1 = (float *) pa;
    float *p2 = (float *) pb;

    if (*p1 < *p2)
       return -1;
    if (*p1 > *p2)
       return 1;

    return 0;
}

static int cmp_double (const void *pa, const void *pb)
{
    double *p1 = (double *) pa;
    double *p2 = (double *) pb;

    if (*p1 < *p2)
       return -1;
    if (*p1 > *p2)
       return 1;

    return 0;
}
