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

#define MAIN
#include "globals.h"

/* local proto */
void set_params();

/* ************************************************************************* */
/* Set up the arguments we are expecting ********************************** */
/* ************************************************************************* */
void set_params()
{
    param.inputfile = G_define_standard_option(G_OPT_R_MAP);

    param.percentile = G_define_option();
    param.percentile->key = "percentile";
    param.percentile->type = TYPE_INTEGER;
    param.percentile->required = NO;
    param.percentile->options = "0-100";
    param.percentile->answer = "90";
    param.percentile->description =
	_("Percentile to calculate (requires extended statistics flag)");

    param.shell_style = G_define_flag();
    param.shell_style->key = 'g';
    param.shell_style->description = _("Print the stats in shell script style");

    param.extended = G_define_flag();
    param.extended->key = 'e';
    param.extended->description = _("Calculate extended statistics");

    return;
}


/* *************************************************************** */
/* **** the main functions for r.univar ************************** */
/* *************************************************************** */
int main(int argc, char *argv[])
{

    unsigned int row, col;	/* counters */
    unsigned int rows, cols;	/*  totals  */

    int val_i;			/* for misc use */
    float val_f;		/* for misc use */
    double val_d;		/* for misc use */
    int first = TRUE;		/* min/max init flag */

    char *infile, *mapset;
    void *raster_row, *ptr;
    RASTER_MAP_TYPE map_type;
    struct Cell_head region;
    int fd;
    struct GModule *module;
    univar_stat *stats;


    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("raster, statistics");
    module->description =
	_
	("Calculates univariate statistics from the non-null cells of a raster map.");

    /* Define the different options */
    set_params();

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);


    infile = param.inputfile->answer;

    mapset = G_find_cell2(infile, "");
    if (mapset == NULL) {
	G_fatal_error(_("Raster map <%s> not found"), infile);
    }

    fd = G_open_cell_old(infile, mapset);
    if (fd < 0)
	G_fatal_error(_("Unable to open raster map <%s>"), infile);

    map_type = G_get_raster_map_type(fd);

    G_get_window(&region);
    rows = region.rows;		/* use G_window_rows(), G_window_cols() here? */
    cols = region.cols;

    stats = create_univar_stat_struct(map_type, cols * rows);
    sscanf(param.percentile->answer, "%i", &stats->perc);

    raster_row = G_calloc(cols, G_raster_size(map_type));

    for (row = 0; row < rows; row++) {
	if (G_get_raster_row(fd, raster_row, row, map_type) < 0)
	    G_fatal_error(_("Reading row %d"), row);

	ptr = raster_row;

	for (col = 0; col < cols; col++) {

	    if (G_is_null_value(ptr, map_type)) {
		ptr = G_incr_void_ptr(ptr, G_raster_size(map_type));
		continue;
	    }


	    if (map_type == CELL_TYPE) {
		val_i = *((CELL *) ptr);

		stats->sum += val_i;
		stats->sumsq += (val_i * val_i);
		stats->sum_abs += abs(val_i);

		if (param.extended->answer)
		    stats->cell_array[stats->n] = val_i;

		if (first) {
		    stats->max = val_i;
		    stats->min = val_i;
		    first = FALSE;
		}
		else {
		    if (val_i > stats->max)
			stats->max = val_i;
		    if (val_i < stats->min)
			stats->min = val_i;
		}
	    }
	    else if (map_type == FCELL_TYPE) {
		val_f = *((FCELL *) ptr);

		stats->sum += val_f;
		stats->sumsq += (val_f * val_f);
		stats->sum_abs += fabs(val_f);

		if (param.extended->answer)
		    stats->fcell_array[stats->n] = val_f;

		if (first) {
		    stats->max = val_f;
		    stats->min = val_f;
		    first = FALSE;
		}
		else {
		    if (val_f > stats->max)
			stats->max = val_f;
		    if (val_f < stats->min)
			stats->min = val_f;
		}
	    }
	    else if (map_type == DCELL_TYPE) {
		val_d = *((DCELL *) ptr);

		stats->sum += val_d;
		stats->sumsq += val_d * val_d;
		stats->sum_abs += fabs(val_d);

		if (param.extended->answer)
		    stats->dcell_array[stats->n] = val_d;

		if (first) {
		    stats->max = val_d;
		    stats->min = val_d;
		    first = FALSE;
		}
		else {
		    if (val_d > stats->max)
			stats->max = val_d;
		    if (val_d < stats->min)
			stats->min = val_d;
		}
	    }
	    ptr = G_incr_void_ptr(ptr, G_raster_size(map_type));
	    stats->n++;
	}
	if (!(param.shell_style->answer))
	    G_percent(row, rows, 2);
    }
    if (!(param.shell_style->answer))
	G_percent(row, rows, 2);	/* finish it off */

    /* create the output */
    print_stats(stats);

    /* release memory */
    free_univar_stat_struct(stats);

    exit(EXIT_SUCCESS);
}
