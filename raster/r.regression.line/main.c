/*
 * r.regression
 *
 *   Calculates linear regression from two raster maps: y = a + b*x
 *   Copyright (C) 2010 by the GRASS Development Team
 *   Author(s): original author Dr. Agustin Lobo
 *              Markus Metz (conversion to C for speed)
 *
 *      This program is free software under the GNU General Public
 *      License (>=v2). Read the file COPYING that comes with GRASS
 *      for details.
 *
 *   This program is a replacement for the r.regression.line script
 *   (the C version is up to 200x faster than the script version)
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/glocale.h>

int main(int argc, char *argv[])
{
    unsigned int r, c, rows, cols;	/*  totals  */
    int map1_fd, map2_fd;
    double sumX, sumY, sumsqX, sumsqY, sumXY;
    double meanX, meanY, varX, varY, sdX, sdY;
    double A, B, R, F;
    long count = 0;
    DCELL *map1_buf, *map2_buf, map1_val, map2_val;
    char *name, *mapset;
    struct Option *input_map1, *input_map2, *output_opt;
    struct Flag *shell_style, *slow;
    struct Cell_head region;
    struct GModule *module;

    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = (_("raster, statistics"));
    module->description =
	_("Calculates linear regression from two raster maps: y = a + b*x.");

    /* Define the different options */
    input_map1 = G_define_standard_option(G_OPT_R_MAP);
    input_map1->key = "map1";
    input_map1->description = (_("Map for x coefficient"));

    input_map2 = G_define_standard_option(G_OPT_R_MAP);
    input_map2->key = "map2";
    input_map2->description = (_("Map for y coefficient"));

    output_opt = G_define_standard_option(G_OPT_F_OUTPUT);
    output_opt->key = "output";
    output_opt->required = NO;
    output_opt->description =
	(_("ASCII file for storing regression coefficients (output to screen if file not specified)."));

    shell_style = G_define_flag();
    shell_style->key = 'g';
    shell_style->description = _("Print in shell script style");

    slow = G_define_flag();
    slow->key = 's';
    slow->description = _("Slower but accurate (applies to FP maps only");

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    name = output_opt->answer;
    if (name != NULL && strcmp(name, "-") != 0) {
	if (NULL == freopen(name, "w", stdout)) {
	    G_fatal_error(_("Unable to open file <%s> for writing"), name);
	}
    }

    G_get_window(&region);
    rows = region.rows;
    cols = region.cols;

    /* open maps */
    mapset = G_find_cell2(input_map1->answer, "");
    if (mapset == NULL) {
	G_fatal_error(_("Raster map <%s> not found"), input_map1->answer);
    }
    map1_fd = G_open_cell_old(input_map1->answer, "");
    if (map1_fd < 0)
	G_fatal_error(_("Unable to open raster map <%s>"),
		      input_map1->answer);

    mapset = G_find_cell2(input_map2->answer, "");
    if (mapset == NULL) {
	G_fatal_error(_("Raster map <%s> not found"), input_map2->answer);
    }
    map2_fd = G_open_cell_old(input_map2->answer, "");
    if (map2_fd < 0)
	G_fatal_error(_("Unable to open raster map <%s>"),
		      input_map2->answer);

    map1_buf = G_allocate_raster_buf(DCELL_TYPE);
    map2_buf = G_allocate_raster_buf(DCELL_TYPE);

    sumX = sumY = sumsqX = sumsqY = sumXY = 0.0;
    meanX = meanY = varX = varY = sdX = sdY = 0.0;
    for (r = 0; r < rows; r++) {
	G_percent(r, rows, 2);
	G_get_raster_row(map1_fd, map1_buf, r, DCELL_TYPE);
	G_get_raster_row(map2_fd, map2_buf, r, DCELL_TYPE);
	for (c = 0; c < cols; c++) {
	    map1_val = map1_buf[c];
	    map2_val = map2_buf[c];
	    if (G_is_null_value(&map1_val, DCELL_TYPE) ||
		G_is_null_value(&map2_val, DCELL_TYPE))
		continue;

	    sumX += map1_val;
	    sumY += map2_val;
	    sumsqX += map1_val * map1_val;
	    sumsqY += map2_val * map2_val;
	    sumXY += map1_val * map2_val;
	    count++;
	}
    }
    G_close_cell(map1_fd);
    G_close_cell(map2_fd);
    G_free(map1_buf);
    G_free(map2_buf);

    B = (sumXY - sumX * sumY / count) / (sumsqX - sumX * sumX / count);
    R = (sumXY - sumX * sumY / count) /
	sqrt((sumsqX - sumX * sumX / count) * (sumsqY - sumY * sumY / count));

    meanX = sumX / count;
    sumsqX = sumsqX / count;
    varX = sumsqX - (meanX * meanX);
    sdX = sqrt(varX);

    meanY = sumY / count;
    sumsqY = sumsqY / count;
    varY = sumsqY - (meanY * meanY);
    sdY = sqrt(varY);

    A = meanY - B * meanX;
    F = R * R / (1 - R * R / count - 2);

    if (shell_style->answer) {
	fprintf(stdout, "a=%f\n", A);
	fprintf(stdout, "b=%f\n", B);
	fprintf(stdout, "R=%f\n", R);
	fprintf(stdout, "N=%ld\n", count);
	fprintf(stdout, "F=%f\n", F);
	fprintf(stdout, "meanX=%f\n", meanX);
	fprintf(stdout, "sdX=%f\n", sdX);
	fprintf(stdout, "meanY=%f\n", meanY);
	fprintf(stdout, "sdY=%f\n", sdY);
    }
    else {
	fprintf(stdout, "y = a + b*x\n");
	fprintf(stdout, "   a (Offset): %f\n", A);
	fprintf(stdout, "   b (Gain): %f\n", B);
	fprintf(stdout, "   R (sumXY - sumX*sumY/N): %f\n", R);
	fprintf(stdout, "   N (Number of elements): %ld\n", count);
	fprintf(stdout, "   F (F-test significance): %f\n", F);
	fprintf(stdout, "   meanX (Mean of map1): %f\n", meanX);
	fprintf(stdout, "   sdX (Standard deviation of map1): %f\n", sdX);
	fprintf(stdout, "   meanY (Mean of map2): %f\n", meanY);
	fprintf(stdout, "   sdY (Standard deviation of map1): %f\n", sdY);
    }

    exit(EXIT_SUCCESS);
}
