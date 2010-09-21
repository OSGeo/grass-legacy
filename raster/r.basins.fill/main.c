
/****************************************************************************
 *
 * MODULE:       r.basins.fill
 *
 * AUTHOR(S):    Dale White - Dept. of Geography, Pennsylvania State U.
 *               Larry Band - Dept. of Geography, University of Toronto
 *
 * PURPOSE:      Generates a raster map layer showing watershed subbasins.
 *
 * COPYRIGHT:    (C) 2005, 2010 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
****************************************************************************/

/*====================================================================*/
/* program to propogate the link label into the hillslope areas;      */
/* processes CELL files only and works on window derived from link    */
/* label map                                                          */

/*====================================================================*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <grass/gis.h>
#include "local_proto.h"
#include <grass/glocale.h>


#define NOMASK 1


int main(int argc, char *argv[])
{
    int partfd;
    int nrows, ncols;
    char drain_name[GNAME_MAX], *drain_mapset;
    char ridge_name[GNAME_MAX], *ridge_mapset;
    char part_name[GNAME_MAX], *part_mapset;
    CELL *drain, *ridge;
    struct Cell_head window;
    int row, col, npass, tpass;
    struct GModule *module;
    struct Option *opt1, *opt2, *opt3, *opt4;

    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("raster, hydrology");
    module->description =
	_("Generates watershed subbasins raster map.");
    
    opt2 = G_define_standard_option(G_OPT_R_INPUT);
    opt2->key = "c_map";
    opt2->description = _("Name of input coded stream network raster map");

    opt3 = G_define_standard_option(G_OPT_R_INPUT);
    opt3->key = "t_map";
    opt3->description = _("Name of input thinned ridge network raster map");

    opt4 = G_define_standard_option(G_OPT_R_OUTPUT);
    opt4->key = "result";

    opt1 = G_define_option();
    opt1->key = "number";
    opt1->type = TYPE_INTEGER;
    opt1->required = YES;
    opt1->description = _("Number of passes through the dataset");
    
    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    sscanf(opt1->answer, "%d", &tpass);

    strcpy(drain_name, opt2->answer);
    drain_mapset = G_find_cell2(drain_name, "");
    if (drain_mapset == NULL)
	G_fatal_error(_("Raster map <%s> not found"), opt2->answer);

    /* this isn't a nice thing to do. G_align_window() should be used first */
    G_get_cellhd(drain_name, drain_mapset, &window);
    G_set_window(&window);

    nrows = G_window_rows();
    ncols = G_window_cols();

    strcpy(ridge_name, opt3->answer);
    ridge_mapset = G_find_cell2(ridge_name, "");
    if (ridge_mapset == NULL)
	G_fatal_error(_("Raster map <%s> not found"), opt3->answer);

    strcpy(part_name, opt4->answer);
    part_mapset = G_find_cell2(part_name, "");
    if (part_mapset != NULL)
	G_fatal_error(_("Raster map <%s> already exists"), opt4->answer);

    drain = read_map(drain_name, drain_mapset, NOMASK, nrows, ncols);
    ridge = read_map(ridge_name, ridge_mapset, NOMASK, nrows, ncols);

    partfd = G_open_cell_new(part_name);
    if (partfd < 0)
	G_fatal_error(_("Unable to create raster map <%s>"), part_name);

    /* run through file and set streams to zero at locations where ridges exist */
    for (row = 0; row < nrows; row++) {
	for (col = 0; col < ncols; col++)
	    if (ridge[row * ncols + col] != 0)
		drain[row * ncols + col] = 0;
    }

    for (npass = 1; npass <= tpass; npass++) {
	for (row = 1; row < nrows - 1; row++) {
	    for (col = 1; col < ncols - 1; col++) {
		if (drain[row * ncols + col] == 0 &&
		    ridge[row * ncols + col] == 0) {
		    if (drain[(row - 1) * ncols + col] != 0 &&
			ridge[(row - 1) * ncols + col] == 0)
			drain[row * ncols + col] =
			    drain[(row - 1) * ncols + col];
		    if (drain[row * ncols + (col - 1)] != 0 &&
			ridge[row * ncols + (col - 1)] == 0)
			drain[row * ncols + col] =
			    drain[row * ncols + (col - 1)];
		}
	    }
	}
	G_message(_("Forward sweep complete"));

	for (row = nrows - 3; row > 1; --row) {
	    for (col = ncols - 3; col > 1; --col) {
		if (drain[row * ncols + col] == 0 &&
		    ridge[row * ncols + col] == 0) {
		    if (drain[(row + 1) * ncols + col] != 0 &&
			ridge[(row + 1) * ncols + col] == 0)
			drain[row * ncols + col] =
			    drain[(row + 1) * ncols + col];
		    if (drain[row * ncols + (col + 1)] != 0 &&
			ridge[row * ncols + (col + 1)] == 0)
			drain[row * ncols + col] =
			    drain[row * ncols + (col + 1)];
		}
	    }
	}
	G_message(_("Reverse sweep complete"));
    }

    /* write out partitioned watershed map */
    for (row = 0; row < nrows; row++)
	G_put_raster_row(partfd, drain + (row * ncols), CELL_TYPE);

    G_message(_("Creating support files for <%s>..."), part_name);
    G_close_cell(partfd);

    exit(EXIT_SUCCESS);
}
