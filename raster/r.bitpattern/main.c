/****************************************************************************
 *
 * MODULE:      r.bitpattern
 * AUTHOR(S):   Radim Blazek
 * PURPOSE:     bit pattern comparison
 *		Functionality:
 *		1. define position: set bit(s) to 1 you want to match
 *		   then convert this position pattern to integer, set pattern=
 *		   parameter with that integer value
 *		2. define pattern *value* which should be in that position:
 *		   first bit pattern of value, convert to integer, set
 *		   patval= parameter
 *
 *		128 64 32 16 8 4 2 1
 *		Example:
 *		1. define position 
 *			xx xx 1x xx
 *			binary: 1000 -> integer: 8 -> pattern=8
 *		2. define value 
 *                      Ex.: we want to check for 0 in that position
 *			xx xx 0x xx
 *			binary: 0000 -> integer: 0 -> patval=0
 *                 if value can be arbitray (0/1), then assume 0 value
 *
 * COPYRIGHT:    (C) 2002 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"

extern CELL f_c(CELL);

int main(int argc, char *argv[])
{
    struct Cell_head cellhd;
    char *name, *result, *mapset;
    void *inrast;
    unsigned char *outrast;
    int nrows, ncols;
    int row, col;
    int infd, outfd;
    int verbose;
    RASTER_MAP_TYPE data_type;
    int pat, patv;
    struct GModule *module;
    struct Option *input, *output, *pattern, *patval;
    struct Flag *flag1;

    G_gisinit(argv[0]);

    module = G_define_module();
    module->description = "GRASS bit pattern.";

    /* Define the different options */

    input = G_define_option();
    input->key = "input";
    input->type = TYPE_STRING;
    input->required = YES;
    input->gisprompt = "old,cell,raster";
    input->description = "Name of an input layer";

    output = G_define_option();
    output->key = "output";
    output->type = TYPE_STRING;
    output->required = YES;
    output->gisprompt = "new,cell,raster";
    output->description = "Name of an output layer";

    pattern = G_define_option();
    pattern->key = "pattern";
    pattern->type = TYPE_INTEGER;
    pattern->required = YES;
    pattern->description = "bit pattern";

    patval = G_define_option();
    patval->key = "patval";
    patval->type = TYPE_INTEGER;
    patval->required = YES;
    patval->description = "pattern value";

    /* Define the different flags */

    flag1 = G_define_flag();
    flag1->key = 'q';
    flag1->description = "Quiet";

    if (G_parser(argc, argv))
	exit(-1);

    name = input->answer;
    result = output->answer;
    verbose = (!flag1->answer);
    pat = atoi(pattern->answer);
    patv = atoi(patval->answer);

    /* find map in mapset */
    mapset = G_find_cell2(name, "");
    if (mapset == NULL)
	G_fatal_error("cell file [%s] not found", name);

    if (G_legal_filename(result) < 0)
	G_fatal_error("[%s] is an illegal name", result);

    /* determine the inputmap type (CELL/FCELL/DCELL) */
    data_type = G_raster_map_type(name, mapset);
    /*if Gispf() error */
    if ((infd = G_open_cell_old(name, mapset)) < 0)
	G_fatal_error("Cannot open cell file [%s]", name);

    if (G_get_cellhd(name, mapset, &cellhd) < 0)
	G_fatal_error("Cannot read file header of [%s]", name);

    /* Allocate input buffer */
    inrast = G_allocate_raster_buf(data_type);

    /* Allocate output buffer, use input map data_type */
    nrows = G_window_rows();
    ncols = G_window_cols();
    outrast = G_allocate_raster_buf(data_type);

    if ((outfd = G_open_raster_new(result, data_type)) < 0)
	G_fatal_error("Could not open <%s>", result);

    for (row = 0; row < nrows; row++) {
	CELL c;

	if (verbose)
	    G_percent(row, nrows, 2);

	/* read input map */
	if (G_get_raster_row(infd, inrast, row, data_type) < 0)
	    G_fatal_error("Could not read from <%s>", name);

	/*process the data */
	for (col = 0; col < ncols; col++) {

	    c = ((CELL *) inrast)[col];
	    /*((CELL *) outrast)[col] = c; */
	    if ((c & pat) == patv)
	   	((CELL *) outrast)[col] = 1;
	    else
		((CELL *) outrast)[col] = 0;

	}

	if (G_put_raster_row(outfd, outrast, data_type) < 0)
	    G_fatal_error("Cannot write to <%s>", result);
    }

    G_free(inrast);
    G_free(outrast);
    G_close_cell(infd);
    G_close_cell(outfd);

    return 0;
}
